;; ## Erlang-style Mailboxes
;;
;; By Dale Schumacher | October 17, 2011
;; Translated to Clojure by Timothy Baldridge | May 7th, 2011
;; 
;; One significant difference between message-passing in Erlang and the pure Actor 
;; Model is the Erlang concept of mailboxes. Actors don’t have mailboxes, at least 
;; not in the sense that they can be queried. Messages simply arrive at some 
;; non-deterministic time after they are asynchronously sent, invoking the current 
;; behavior of the actor. However, in Erlang, messages are delivered to the mailbox 
;; of a process, and the process must execute a receive to query for a message [1].

;; The semantics of Erlang mailboxes, and specifically the selective receive used 
;; to choose specific messages, significantly affects the way programs are written.
;; As Mark Miller says in his thesis, “a typical programming pattern is for a 
;; process to block waiting for a reply, and to remain unresponsive to further 
;; requests while blocked” [2].

;; In pure-actor languages, like Humus, messages are processed serially, but in 
;; a non-deterministic arrival order. Actors never block, so they are always 
;; responsive to message arrivals. When, in the process of handling a message, 
;; an actor needs to make a request of another actor, it is common to create a 
;; new customer actor to await the reply. This is similar to use of call-back 
;; functions in many asynchronous processing systems.

;; If an Erlang-style mailbox is a better fit to the problem at hand, it is 
;; possible to implement it using a group of actors to represent the mailbox. 
;; We will explore the implementation of a somewhat more general mailbox 
;; mechanism that can be used with multiple senders and receivers. A receiver 
;; specifies a pattern that is used to either select a message from the mailbox, 
;; or defer the request until a matching message is sent to the mailbox 
;; (logically “blocking” the receiver).


;; ## Mailbox Behavior

;; The general structure of our mailbox is a queue of pending send/receive requests
;; waiting to be matched. Each pending request is represented by an actor. The 
;; root and end actors serve as sentinels for the front and back of the pending 
;; request queue. Our initial mailbox behavior implement our usual 
;; lazy-initialization pattern, waiting for the first request to trigger 
;; construction of an initially empty mailbox.
(defbeh mailbox-beh 
    ([msg] (create next :with end-mailbox-beh self)
           (become root-mailbox-beh next)
           (send msg self)))
    

;; As messages are matched, we will want to remove the actor representing the 
;; corresponding send/receive request. A :prune request communicates a new 
;; successor to use if our current successor is an actor to be removed. Any other
;; requests are forwarded on to the next actor in the mailbox queue.

(defbeh root-mailbox-beh [nxt]
    ([next :prune next-act] (become root-mailbox-beh next-act))
    ([_] (send msg to nxt)))

;; If the end of the mailbox is reached without finding a matching message, 
;; the send/receive should be added at the end of the queue as a pending request. 
;; Receive requests contain a predicate function used to decide if a message is a 
;; match. If no match is found, a pending receive request is queued, and no reply 
;; is sent. Although no actors actually block, the customer of the receive request 
;; will not see a reply until a matching message is sent to the mailbox, logically 
;; blocking the receiver. Send requests always reply with the identity of the 
;; mailbox root. This serves as a kind of synchronization signal, indicating that 
;; the message has either been delivered, or has been successfully queued in the 
;; mailbox.
(defbeh end-mailbox-beh [root]
    ([cust :recv pred]
        (create next end-mailbox-beh root)
        (become recv-mailbox-beh root cust pred next))
    ([cust :send m]
        (create next end-mailbox-beh root)
        (become send-mailbox-beh root m next)))


;; An actor representing a pending receive request is listening for a send 
;; request that matches the predicate specified by the receiver. When such a 
;; request arrives, the message sent is passed to the receiving customer, the 
;; current actor becomes a no-op, a :prune request is sent to the root to eliminate
;; the current actor, and the sending customer is sent the root as a sync-signal. 
;; A :prune message may also arrive, requesting the removal of our successor. 
;; Any other requests are forwarded on to the next actor in the mailbox queue.
(defbeh recv-mailbox-beh [root cust pred next]
    ([cust :send (m :when pred)]
           (send cust m)
           (become skip-mailbox-beh next)
           (send root self :prune next)
           (send cust root))
    ([cust :send m]
           (send msg next))
    ([$next :prune next']
           (become recv-mailbox-beh root cust pred next'))
    ([_] (send msg :to next)))

;; An actor representing a pending send request is listening for a receive 
;; request with a predicate that matches the pending message. When such a request 
;; arrives, the message sent is passed to the receiving customer, the current 
;; actor becomes a no-op, and a #prune request is sent to the root to eliminate 
;; the current actor. A #prune message may also arrive, requesting the removal 
;; of our successor. Any other requests are forwarded on to the next actor in 
;; the mailbox queue.

LET send_mailbox_beh(root, m, next) = \msg.[
	CASE msg OF
	(cust, #recv, pred) : [
		IF $pred(m) = TRUE [
			SEND m TO cust
			BECOME skip_mailbox_beh(next)
			SEND (SELF, #prune, next) TO root
		] ELSE [
			SEND msg TO next
		]
	]
	($next, #prune, next') : [
		BECOME send_mailbox_beh(root, m, next')
	]
	_ : [ SEND msg TO next ]
	END
]
When a pending send/receive request is matched, the corresponding actor takes on the no-op behavior. While is this state, only #prune messages are processed. All other requests are forwarded on to the next actor in the mailbox queue. Note that the no-op actor will never see its own #prune message since its predecessor will handle it by eliminating this actor from the queue.

LET skip_mailbox_beh(next) = \msg.[
	CASE msg OF
	($next, #prune, next') : [
		BECOME skip_mailbox_beh(next')
	]
	_ : [ SEND msg TO next ]
	END
]

Test Fixture

We can exercise the mailbox code with a simple scenario. Three #send requests and two #recv requests are sent the mailbox. The #send requests send the symbols #foo, #bar, and #baz. The #recv requests select a message that is equal to #foo, and one not equal to #foo. It is non-deterministic whether #bar or #baz will be matched by the not equal to #foo predicate. The unmatched message will remain in the queue.

CREATE mbox WITH mailbox_beh
SEND (println, #recv, \m.not(eq(m, #foo))) TO mbox
SEND (println, #send, #foo) TO mbox
SEND (println, #send, #bar) TO mbox
SEND (println, #send, #baz) TO mbox
SEND (println, #recv, \m.eq(m, #foo)) TO mbox
The output shown by println will be the two messages matched by the #recv requests, and three references to the mbox actor, one for each #send request.



