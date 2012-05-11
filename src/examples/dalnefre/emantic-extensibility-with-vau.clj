; ## Semantic Extensibility with Vau 
; By Dale Schumacher | December 18, 2011
; Translated by Timothy Baldridge April 9th, 2012

; John
; Shutt has reformulated the foundations of LISP/Scheme [1]. Observing that Lambda
; is a primitive applicative constructor, he proposes Vau as a primitve operative
; constructor instead. This changes our focus from implicit evaluation to explicit
; evaluation. Applicatives evaluate their operands before evaluating the
; combination. Operatives act directly on their (unevaluated) operands, possibly
; evaluating them selectively.
; 
; Building on this foundation, he has created the Kernel language [2]. Kernel
; retains the look-and-feel of LISP/Scheme, while providing well-behaved semantics
; and powerful semantic extension capabilities. A key feature of Kernel is the
; clean simplicity of its syntax and semantics. The syntax consist of only three
; elements; Constants, Symbols and Pairs. Constants evaluate to themselves. Pairs
; represent combinations, which can be treated as data (forming lists) or be
; evaluated based on the applicative/operative in first position. Symbols
; represent variables which evaluate to values based on environment bindings.
; Bindings to primitives provide the bridge to the rich semantics of Kernel.
; 
; Primitives are available to construct first-class objects for all of the
; semantic domains (types) required to describe Kernel meta-circularly. This means
; that we can describe Kernel semantic objects using only Constants, Symbols and
; Pairs, but the resulting objects are opaque. It is this opacity that prevents
; primitive operatives from violating hygiene, exposing or interfering with the
; internal structure of semantics objects.
; 
; In the previous article we constructed an actor-based evaluator for a
; Kernel-inspired language core. The actor-based implementation exhibits pervasive
; fine-grained concurrency. However, we did not expose enough primitive
; functionality for the language to describe itself. We will correct that defect
; here. Besides striving for conceptual completeness, exposing all the semantic
; primitives provides a powerful foundation for extending the language.
; 
; Type
; 
; Our actor-based evaluator implements each semantic value as an actor that
; responds to #eval requests (after Hewitt [3]). As previously discussed, actors
; naturally favor an object-oriented approach to method dispatch. Therefore, each
; actor’s behavior specifies how it responds to #eval, and other operational
; requests. Languages like LISP, Scheme and Kernel take a functional approach, but
; do not have type-based overloading of functions. Therefore, type predicates are
; required to implement algorithmic variance based on the types of parameters.
; 
; Abstract Type
; 
; We will extend the behavior of the actors implementing our semantic values to
; support a #type_eq request, which returns True if the actor is of the specified
; type, and False otherwise. Actor behaviors are specified by behavior-generating
; functions. We define a behavior wrapper Type that implements the behavior for
; #type_eq requests.

(defbeh Type [this behavior] 
    ([cust [:type_eq $this]] (post cust true))
    ([cust [:type_eq _]] (post cust false))
    ([msg] (behavior msg))) ; delegate to behavior



LET Type(this, behavior) = \(cust, req).[
	CASE req OF
	(#type_eq, $this) : [ SEND True TO cust ]
	(#type_eq, _) : [ SEND False TO cust ]
	_ : behavior(cust, req)  # delegate to behavior
	END
]

; The Type behavior-generating function takes a reference to this type, and a
; behavior function that specifies the rest of the behavior. You could think of
; Type as equivalent to an abstract super-class, with behavior as the sub-class.
; 
; Most semantic values are types of constants, and thus evaluate to themselves. We
; support this common behavior with another behavior wrapper SeType that
; implements self-evaluation behavior in response to #eval requests.


(defn SeType [this behvaior]
    (Type this (beh [cust [:eval, _]] (send cust self@)
                    [cust _] (behavior msg))))

LET SeType(this, behavior) = Type(this, \(cust, req).[
	CASE req OF
	(#eval, _) : [ SEND SELF TO cust ]
	_ : behavior(cust, req)  # delegate to behavior
	END
])
The SeType behavior-generating function uses Type to construct a behavior which handles #type_eq, but further specifies its own behavior to handle #eval requests and delegate to a deeper behavior for unrecognized requests.


Unit

Let’s make this all concrete. The Inert object is the sole member of a type we will call Unit. It has no interesting behavior beyond responding to #type_eq (provided by Type) and self-evaluation (provided by SeType). We only need to specify a behavior to handle unrecognized requests.

LET Unit() = SeType(Unit, \(cust, req).[
	THROW (#Not-Understood, SELF, req)
])
CREATE Inert WITH Unit()
Notice how the SeType constructor is parameterized with the Unit type being defined. Since Inert doesn’t support any additional requests, the Unit type specifies that it will signal an error if it receives an unrecognized request.


Operative

The Oper type specifies the behavior of operatives. Operatives are self-evaluating, so we extend from SeType. Operatives also handle #comb requests, to perform combination.

LET Oper(comb_beh) = SeType(Oper, \(cust, req).[
	CASE req OF
	(#comb, opnds, env) : comb_beh(cust, opnds, env)
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
All operatives have Oper type, but each may perform combination in a different way. The comb_beh function defines the combination operation for a specific operative. Unrecognized requests signal an error.


Applicative

The Appl type specifies the behavior of applicatives. Applicatives are self-evaluating, so we extend from SeType. Applicatives also handle #comb and #unwrap requests.

LET Appl(comb) = SeType(Appl, \(cust, req).[
	CASE req OF
	(#comb, opnds, env) : [
		SEND (k_args, #map, #eval, env) TO opnds
		CREATE k_args WITH \args.[
			CREATE expr WITH Pair(comb, args)
			SEND (cust, #eval, env) TO expr
		]
	]
	#unwrap : [ SEND comb TO cust ]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
The applicative behavior is the same as previously described. For #comb, the operands are evaluated (concurrently) and the resulting arguments are passed to the underlying combiner. For #unwrap, we return the underlying combiner. We’ve simply factored out the common behavior of self-evaluation and type-checking.


Boolean

The Boolean type has exactly two members; True and False. Boolean constants are self-evaluating, so we extend from SeType. They also handle #if requests.

LET Boolean(if_beh) = SeType(Boolean, \(cust, req).[
	CASE req OF
	(#if, cnsq, altn, env) : if_beh(cust, cnsq, altn, env)
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
CREATE True WITH Boolean(\(cust, cnsq, _, env).[
	SEND (cust, #eval, env) TO cnsq
])
CREATE False WITH Boolean(\(cust, _, altn, env).[
	SEND (cust, #eval, env) TO altn
])
Since True and False handle #if requests differently, the Boolean type constructor is parameterized with if_beh. Each of the two concrete Boolean values provides its own #if handler. True evaluates the consequence cnsq, and False evaluates the alternative altn.


Type-Predicate

A type-predicate is an applicative with the external form (type? . objects). It evaluates to True if all of the objects are of the specified type, and False otherwise. We define an applicative type-predicate constructor Appl_p? that takes a type parameter. An applicative wraps an operative (which may be extracted with #unwrap). Our applicative type-predicates are just wrappers around operative type-predicates constructed by Oper_&p?.

LET boolean_and(p, q) = (IF $p = $True (q) ELSE (False))
LET Oper_&p?(type) = Oper(\(cust, opnds, env).[
	SEND (cust, #foldl, True, boolean_and, #type_eq, type) TO opnds
])
LET Appl_p?(type) = Appl(NEW Oper_&p?(type))
Iteration through the operands is implemented with a new #foldl request, which takes an accumulator value, a combination function, and a nested request. The nested request is sent to each element of the operand list from left to right. The combination function is called with the accumulator value and the response from the nested request, producing a new accumulator value. When Nil is reached, the accumulated value is returned to the original customer. In the case of type predicates, the combiner function is boolean_and and the nested request is #type_eq.

CREATE Appl_inert? WITH Appl_p?(Unit)
CREATE Appl_applicative? WITH Appl_p?(Appl)
CREATE Appl_operative? WITH Appl_p?(Oper)
CREATE Appl_boolean? WITH Appl_p?(Boolean)
Using our new type-predicate constructor Appl_p?, we define type-predicates for the Unit, Appl, Oper and Boolean types defined so far.


Null

The Nil object is the sole member of type Null. It is self-evaluating, so we extend from SeType. In addition to behavior described previously, we extend Nil to handle #as_pair and #foldl requests.

LET Null() = SeType(Null, \(cust, req).[
	CASE req OF
	#as_pair : [ SEND NIL TO cust ]
	#as_tuple : [ SEND NIL TO cust ]
	(#match, $Nil, env) : [ SEND Inert TO cust ]
	(#map, req') : [ SEND (cust, req') TO SELF ]
	(#foldl, zero, oplus, req') : [ SEND zero TO cust ]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
CREATE Nil WITH Null()
CREATE Appl_null? WITH Appl_p?(Null)
We use #as_pair to destructure a list into its components (similar to #as_tuple), so Nil returns NIL (the empty list in Humus). For #foldl we simply return the accumulated value zero.


Pair

The Pair type carries a lot of responsibility. Since it is the primary compound structured type, it handles traversal logic for a variety of operations. It is not self-evaluating, so we extend Type instead of SeType, and provide our own behavior for #eval requests. In addition to behavior described previously, we extend Pair to handle #as_pair and #foldl requests.

LET Pair(left, right) = Type(Pair, \(cust, req).[
	CASE req OF
	(#eval, env) : [
		SEND (k_comb, #eval, env) TO left
		CREATE k_comb WITH \comb.[
			SEND (cust, #comb, right, env) TO comb
		]
	]
	#as_pair : [ SEND (left, right) TO cust ]
	#as_tuple : [
		SEND (k_tuple, #as_tuple) TO right
		CREATE k_tuple WITH \tuple.[
			SEND (left, tuple) TO cust
		]
	]
	(#match, value, env) : [
		CREATE fork WITH fork_beh(k_pair, value, value)
		SEND (
			(#match_left, left, env),
			(#match_right, right, env)
		) TO fork
		CREATE k_pair WITH \($Inert, $Inert).[
			SEND Inert TO cust
		]
	]
	(#match_left, ptree, env) : [
		SEND (cust, #match, left, env) TO ptree
	]
	(#match_right, ptree, env) : [
		SEND (cust, #match, right, env) TO ptree
	]
	(#map, req') : [
		CREATE fork WITH fork_beh(k_pair, left, right)
		SEND (req', req) TO fork
		CREATE k_pair WITH \(head, tail).[
			CREATE pair WITH Pair(head, tail)
			SEND pair TO cust
		]
	]
	(#foldl, zero, oplus, req') : [
		SEND (k_one, req') TO left
		CREATE k_one WITH \one.[
			SEND (cust, #foldl,
				oplus(zero, one), oplus, req')
			TO right
		]
	]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
CREATE Appl_pair? WITH Appl_p?(Pair)
We use #as_pair to destructure a list into its components (similar to #as_tuple), so Pair returns its left and right components (as a pair in Humus). For #foldl we send the nested request req’ to the left component. The oplus function combines the accumulator zero with the reply one to produce a new accumulated value. Finally, a new #foldl request, with the accumulated value, is sent to the right component. Note that #foldl processes list elements sequentially from left to right, unlike #map which processes list elements in parallel (using the previously defined concurrent fork behavior).


Symbol

The Symbol type specifies the behavior of symbolic variables. Symbols are not self-evaluating, so we extend Type instead of SeType, and provide our own behavior for #eval requests (looking up their value in the environment). A #match request causes binding of the symbol to a value in the environment.

LET Symbol(name) = Type(Symbol, \(cust, req).[
	CASE req OF
	(#eval, env) : [ SEND (cust, #lookup, SELF) TO env ]
	(#match, value, env) : [
		SEND (cust, #bind, SELF, value) TO env
	]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
CREATE Appl_symbol? WITH Appl_p?(Symbol)

Any

The Ignore object is the sole member of a type we will call Any. It is self-evaluating, so we extend from SeType. It also handles #match requests. A #match request is always successful (matching any value), but does not cause any binding.

LET Any() = SeType(Any, \(cust, req).[
	CASE req OF
	(#match, _) : [ SEND Inert TO cust ]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
CREATE Ignore WITH Any()
CREATE Appl_ignore? WITH Appl_p?(Any)

Environment

Environments map Symbols to values. They are first-class semantic objects, yet their structure is opaque. They cannot be decomposed by operatives. This is one of the key principles underlying the safety of extensibility in Kernel. Operatives can manipulate syntax, but cannot violate the encapsulation of semantic values. This gives us the freedom to implement environments using more efficient native mechanisms of Humus, including functional representation of bindings and lazy initialization of environment actors.

LET map_empty = \_.?
LET map_bind(map, key, value) = \lookup.(
	CASE lookup OF
	$key : value
	_ : map(lookup)
	END
)
LET Env(parent) = \(cust, req).[
	BECOME Env_scope(parent, map_empty)
	SEND (cust, req) TO SELF
]
The behavior of environments was described previously. We have refactored the implementation to extend from SeType, since environments are self-evaluating instances of the Env type.

LET Env_scope(parent, map) = SeType(Env, \(cust, req).[
	CASE req OF
	(#lookup, key) : [
		CASE map(key) OF
		? : [ SEND (cust, req) TO parent ]
		value : [ SEND value TO cust ]
		END
	]
	(#bind, key, value) : [
		BECOME Env_scope(parent, map_bind(map, key, value))
		SEND Inert TO cust  # new binding
	]
	_ : [ SEND (cust, req) TO parent ]
	END
])
CREATE Env_empty WITH SeType(Env, \(cust, req).[
	CASE req OF
	(#lookup, key) : [ THROW (#Undefined, key) ]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
CREATE Appl_environment? WITH Appl_p?(Env)

Encapsulation

We have shown how to create primitive encapsulation types. In order to support greater extensibility, we expose a facility for creating user-defined encapsulation types. That facility is built on an ObjType behavior-generating function, extending from SeType and adding a handler for #content requests.

LET ObjType(this, content) = SeType(this, \(cust, req).[
	CASE req OF
	(#content, $this) : [ SEND content TO cust ]
	_ : [ THROW (#Not-Understood, SELF, req) ]
	END
])
A user-defined type encapsulates an arbitrary content value. A #content request is used to retrieve that value, but only if this is the correct type.

The applicative with external form (make-encapsulation-type) creates a new user-defined encapsulation type. It returns a list of three applicatives; an encapsulator, a type-predicate and a decapsulator. Each call to Appl_make-encapsulation-type produces a new set of applicatives representing a new unique user-defined encapsulation type.

LET pr(x, y) = (NEW Pair(x, y))
CREATE Appl_make-encapsulation-type WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \NIL.[  # no arguments
			LET T(content) = ObjType(T, content)
			CREATE e WITH Appl_e(T)
			CREATE p? WITH Appl_p?(T)
			CREATE d WITH Appl_d(T)
			SEND pr(e, pr(p?, pr(d, Nil))) TO cust
		]
	])
)
The applicative Appl_make-encapsulation-type wraps an anonymous operative that takes no arguments. An encapsulation type constructor T is created with ObjType, which is then used to create a matched set of encapsulator e, type-predicate p? and decapsulator d applicatives. The predicate is created with Appl_p?, as we have done for all of our primitive types. The encapsulator constructor Appl_e and decapsulator constructor Appl_d are defined below. Finally, a three-element list of Pairs is created (using the pr helper).

LET Appl_e(type) = Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(content, NIL).[
			CREATE object WITH type(content)
			SEND object TO cust
		]
	])
)
The applicative encapsulator constructor Appl_e takes a type parameter which is the object constructor. When the encapsultor is called, its single argument is the content parameter used to construct a new instance of the encapsulation type. The contents of an encapsulation type can only be retrieved by the matching decapsulator.

LET Appl_d(type) = Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(object, NIL).[
			SEND (cust, #content, type) TO object
		]
	])
)
The applicative decapsulator constructor Appl_d takes a type parameter which is used to validate access to the contents of an object. When the decapsulator is called, its single argument is an instance of the encapsulation type (or an error is signalled). A #content request is sent to the object, along with the encapsulation type identifier. Since the type is never directly exposed, it serves as an authorization token for access to the contents of this object type. By controlling the visibility of the encapsulator and decapsulator, we can control what code has the capability to created and/or access the contents of each encapsulation type.


Library

A library of primitive operatives and applicatives provides access to all the semantic primitives required to describe the language meta-circularly, as well as forming a foundation for language extension. The goal is to allow implementation of new semantic features which are indistinguishable from built-in functionality.


&lambda

The operative Oper_&lambda has the external representation (&lambda ptree . body). This is the most commonly used combiner constructor. It builds traditional applicative combiners. It is semantically equivalent to (wrap (&vau ptree #ignore . body)), but is implemented more directly here for both clarity and performance.

CREATE Oper_&lambda WITH Oper(\(cust, opnds, static).[
	SEND (k_args, #as_pair) TO opnds
	CREATE k_args WITH \(ptree, body).[
		CREATE appl WITH Lambda(static, ptree, body)
		SEND appl TO cust
	]
])
The operands to Oper_&lambda are a formal parameter tree ptree and a list of body expressions. These parameters, along with the static environment, are captured by the Lambda constructor.

LET Lambda(static, ptree, body) = Appl(
	NEW Oper(\(cust, args, _).[
		CREATE local WITH Env(static)
		SEND (k_eval, #match, args, local) TO ptree
		CREATE k_eval WITH \$Inert.[
			# body is a list of expressions
			SEND (cust, #foldl, 
				Inert, (\(x,y).y), #eval, local) 
			TO body
		]
	])
)
Lambda constructs an applicative that contains an operative. The applicative wrapper evaluates the operands to produce arguments. A new empty local environment is created, derived from the static environment. The arguments args are matched to the formal parameter tree ptree, binding variables in the local environment. As a generalization of the previous implementation, the body is a List of expressions, rather than a single expression. A #foldl request is sent to the body, causing evaluation of each expression (in the local environment) sequentially from left to right. The combining function (\(x,y).y) retains the value of the last expression evaluated.


&vau

The operative Oper_&vau has the external representation (&vau vars evar . body). This is the fundamental primitive combiner constructor on which Kernel is based. It builds an operative combiner (fexpr) that manipulates the syntactic structure of its operands.

CREATE Oper_&vau WITH Oper(\(cust, opnds, static).[
	SEND (k_pair, #as_pair) TO opnds
	CREATE k_pair WITH \(vars, opnds').[
		SEND (SELF, #as_pair) TO opnds'
		BECOME \(evar, body).[
			CREATE comb WITH
				Vau(static, pr(vars, evar), body)
			SEND comb TO cust
		]
	]
])
The operands to Oper_&vau are a formal parameter tree vars, and environment parameter evar and a list of body expressions. These parameters, along with the static environment, are captured by the Vau constructor. Note that vars and evar are paired to create a combined formal parameter tree.

LET Vau(static, ptree, body) = Oper(\(cust, opnds, dynamic).[
	CREATE local WITH Env(static)
	SEND (k_eval, #match, pr(opnds, dynamic), local) TO ptree
	CREATE k_eval WITH \$Inert.[
		# body is a list of expressions
		SEND (cust, #foldl,
			Inert, (\(x,y).y), #eval, local) TO body
	]
])
Vau constructs an operative to process unevaluated operands. When the operative is called, a new empty local environment is created, derived from the static environment. The operands opnds and dynamic environment are paired and matched to the combined formal parameter tree ptree, binding variables in the local environment. As a generalization of the previous implementation, the body is a List of expressions, rather than a single expression. A #foldl request is sent to the body, causing evaluation of each expression (in the local environment) sequentially from left to right. The combining function (\(x,y).y) retains the value of the last expression evaluated.


wrap/unwrap

The applicative Appl_wrap has the external representation (wrap combiner). It builds an applicative wrapper (inducing argument evaluation) for another combiner (usually an operative). See &lambda for an example of how wrap might be used.

CREATE Appl_wrap WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(comb, NIL).[
			CREATE appl WITH Appl(comb)
			SEND appl TO cust
		]
	])
)
The applicative Appl_unwrap has the external representation (unwrap applicative). It returns the combiner (usually an operative) underlying an applicative.

CREATE Appl_unwrap WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(appl, NIL).[
			SEND (cust, #unwrap) TO appl
		]
	])
)
Note that, since the #unwrap request is only understood by applicatives, this will raise an error if an attempt is made to unwrap anything but an applicative.


eq?

The applicative Appl_eq? has the external representation (eq? first . rest). It returns True if each object in rest is the same object as first. Otherwise it returns False.

CREATE Appl_eq? WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(first, rest).[
			LET eq*(args) = (
				CASE args OF
				NIL : True
				(h, t) : (
					CASE h OF
					$first : eq*(t)
					_ : False
					END
				)
				END
			)
			SEND eq*(rest) TO cust
		]
	])
)
When Appl_eq? is called, it creates an n-tuple of its evaluated arguments. The first argument is used to define a recursive matching function eq*, which traverses the rest of the args.


&if

The operative Oper_&if has the external representation (&if test consequence alternative). If test evaluates to True, the consequence is evaluated and its value returned. If test evaluates to False, the alternative is evaluated and its value returned.

CREATE Oper_&if WITH Oper(\(cust, opnds, env).[
	SEND (k_args, #as_tuple) TO opnds
	CREATE k_args WITH \(test, cnsq, altn, NIL).[
		SEND (k_bool, #eval, env) TO test
		CREATE k_bool WITH \bool.[
			SEND (cust, #if, cnsq, altn, env) TO bool
		]
	]
])
The selection of which expression to evaluate, cnsq or altn, is made by bool, the Boolean result of evaluating test (c.f.: type Boolean).


cons

The applicative Appl_cons has the external representation (cons car cdr). It constructs a new Pair with left component car and right component cdr.

CREATE Appl_cons WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(a, d, NIL).[
			CREATE pair WITH Pair(a, d)
			SEND pair TO cust
		]
	])
)

eval

The applicative Appl_eval has the external representation (eval expression environment). It evalutes the expression in the specified environment. This allows us to construct a syntactic representation of an expression and evaluate it in a controlled environment.

CREATE Appl_eval WITH Appl(
	NEW Oper(\(cust, opnds, _).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \(expr, env, NIL).[
			SEND (cust, #eval, env) TO expr
		]
	])
)

make-environment

The applicative Appl_make-environment has external representations (make-environment) and (make-environment parent). The first form constructs a new empty environment. The second form constructs a new empty environment, derived from a parent environment. As previously described, only the immediate environment is mutable. All environments in the parent chain are protected from mutation.

CREATE Appl_make-environment WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \parents.[  # only 0 or 1 supported!
			CASE parents OF
			NIL : [
				SEND NEW Env(Env_empty) TO cust
			]
			(parent, NIL) : [
				SEND NEW Env(parent) TO cust
			]
			END
		]
	])
)

get-current-environment

The applicative Appl_get-current-environment has the external representation (get-current-environment). It simply returns the current (dynamic) environment. This can be used as the argument to make-environment to construct an environment which protects the current environment from mutation.

CREATE Appl_get-current-environment WITH Appl(
	NEW Oper(\(cust, opnds, env).[
		SEND (k_args, #as_tuple) TO opnds
		CREATE k_args WITH \NIL.[  # no arguments
			SEND env TO cust
		]
	])
)

&define!

The operative Oper_&define! has the external representation (&define! ptree expression). It evaluates the expression and matches the resulting value with the formal parameter tree ptree to create or mutate bindings in the immediate environment.

CREATE Oper_&define! WITH Oper(\(cust, opnds, env).[
	SEND (k_args, #as_tuple) TO opnds
	CREATE k_args WITH \(ptree, expr, NIL).[
		SEND (k_value, #eval, env) TO expr
		CREATE k_value WITH \value.[
			SEND (cust, #match, value, env) TO ptree
		]
	]
])

list

The applicative Appl_list has the external representation (list . expressions). It evaluates the expressions concurrently and returns a List of the resulting values.

CREATE Appl_list WITH Appl(
	NEW Oper(\(cust, args, _).[
		SEND args TO cust
	])
)
Since evaluating a list of operands to produce arguments is the defining characteristic of an Applicative, this applicative need only return the resulting list to the customer. However, we still must nest an operative inside the applicative, since the applicative may be unwraped.


&sequence

The operative Oper_&sequence has the external representation (&sequence . body). It evaluates each expression in body sequentially, from left to right. The value of the last expression evaluated is returned, or Inert if the body is empty.

CREATE Oper_&sequence WITH Oper(\(cust, opnds, env).[
	SEND (cust, #foldl, Inert, (\(x,y).y), #eval, env) TO opnds
])
This #foldl idiom is used to evaluate body expressions in &lambda and &vau.


Ground Environment

The ground environment binds a set of known symbols to a library of primitive operatives and applicatives. These symbol bindings provide the bridge between syntactic descriptions (which use symbols) and semantic objects (which are encapsulated).

CREATE Symbol_boolean? WITH Symbol(#boolean?)
CREATE Symbol_eq? WITH Symbol(#eq?)
CREATE Symbol_symbol? WITH Symbol(#symbol?)
CREATE Symbol_inert? WITH Symbol(#inert?)
CREATE Symbol_&if WITH Symbol(#&if)
CREATE Symbol_pair? WITH Symbol(#pair?)
CREATE Symbol_null? WITH Symbol(#null?)
CREATE Symbol_cons WITH Symbol(#cons)
CREATE Symbol_environment? WITH Symbol(#environment?)
CREATE Symbol_ignore? WITH Symbol(#ignore?)
CREATE Symbol_eval WITH Symbol(#eval)
CREATE Symbol_make-environment WITH Symbol(#make-environment)
CREATE Symbol_&define! WITH Symbol(#&define!)
CREATE Symbol_applicative? WITH Symbol(#applicative?)
CREATE Symbol_operative? WITH Symbol(#operative?)
CREATE Symbol_&vau WITH Symbol(#&vau)
CREATE Symbol_wrap WITH Symbol(#wrap)
CREATE Symbol_unwrap WITH Symbol(#unwrap)
CREATE Symbol_&lambda WITH Symbol(#&lambda)
CREATE Symbol_get-current-environment
	WITH Symbol(#get-current-environment)
CREATE Symbol_list WITH Symbol(#list)
CREATE Symbol_&sequence WITH Symbol(#&sequence)
CREATE Symbol_make-kernel-standard-environment
	WITH Symbol(#make-kernel-standard-environment)
CREATE Env_ground WITH Env_scope(Env_empty, \lookup.(
	CASE lookup OF
	# primitives
	$Symbol_boolean? : Appl_boolean?
	$Symbol_eq? : Appl_eq?
	$Symbol_symbol? : Appl_symbol?
	$Symbol_inert? : Appl_inert?
	$Symbol_&if : Oper_&if
	$Symbol_pair? : Appl_pair?
	$Symbol_null? : Appl_null?
	$Symbol_cons : Appl_cons
	$Symbol_environment? : Appl_environment?
	$Symbol_ignore? : Appl_ignore?
	$Symbol_eval : Appl_eval
	$Symbol_make-environment : Appl_make-environment
	$Symbol_&define! : Oper_&define!
	$Symbol_applicative? : Appl_applicative?
	$Symbol_operative? : Appl_operative?
	$Symbol_&vau : Oper_&vau
	$Symbol_wrap : Appl_wrap
	$Symbol_unwrap : Appl_unwrap
	# extensions
	$Symbol_&lambda : Oper_&lambda
	$Symbol_get-current-environment : Appl_get-current-environment
	$Symbol_list : Appl_list
	$Symbol_&sequence : Oper_&sequence
	$Symbol_make-kernel-standard-environment : NEW Appl(
		NEW Oper(\(cust, opnds, _).[
			SEND (k_args, #as_tuple) TO opnds
			CREATE k_args WITH \NIL.[  # no arguments
				CREATE env WITH Env(Env_ground)
				SEND env TO cust
			]
		])
	)
	_ : ?
	END
))
The Env_ground environment contains all of the standard bindings from symbols to semantic objects. This environment is immutable. Expressions are normally evaluated in a child of this environment, such as one constructed by the applicative (make-kernel-standard-environment). The Kernel language description ([2], starting with section 5) defines many additional constructs that are part of the ground environment, but they are all derived from the primitives we have already illustrated.


Banker’s Queue

As a demonstration of extensibility in our target language, we will re-implement the Banker’s Queue datatype shown previously. This implementation makes use of some Kernel features not available in traditional LISP/Scheme dialects, such as encapsulated datatypes and &provide! to control visibility of definitions.

(&provide! (new-q q? q-empty? q-put q-take)
	(&define! (E q? D) (make-encapsulation-type))
	(&define! new-q
		(&lambda ()
			(E (cons () ()))))
	(&define! q-empty?
		(&lambda (obj)
			((&lambda ((p . q))
				(null? p))
			(D obj))))
	(&define! push-pop
		(&lambda (r s)
			(&if (null? s)
				r
				(push-pop
					(cons (car s) r)
					(cdr s)))))
	(&define! reverse
		(&lambda (s)
			(push-pop () s)))
	(&define! q-norm
		(&lambda (p q)
			(&if (null? p)
				(E (cons (reverse q) ()))
				(E (cons p q)))))
	(&define! q-put
		(&lambda (obj x)
			((&lambda ((p . q))
				(q-norm p (cons x q)))
			(D obj))))
	(&define! q-take
		(&lambda (obj)
			((&lambda (((x . p) . q))
				(list x (q-norm p q)))
			(D obj)))))
As shown here, &provide! takes a list of symbols to be exported and a sequence of expressions. The expressions are evaluated in a local environment, derived from the current environment. Afterwards, the symbols listed are exported into the current environment. This mechanism is used to restrict the visibility of the type created by make-encapsulation-type, and hide various helper-functions used in the implementation.

(new-q) creates an empty queue. (q? . objects) is a type predicate for objects created with new-q. (q-empty? queue) returns True iff queue is empty. (q-put queue item) returns a new queue value with item added to the end of the queue. (q-take queue) returns a list (item queue’) consisting of the item taken from the front of the queue and the new queue’ value. Note that queues are immutable values. q-put and q-take always return new queue values. Also, due to encapsulation, the internal structure of a queue is only accessible to the implementation procedures.


Conclusion

Vau abstraction provides a solid foundation for semantic extensibility. By shifting our perspective from implicit evaluation to explicit evaluation, operatives (e.g.: macros and special-forms) and applicatives (traditional procedures) can be expressed within the same consistent framework. An actor-based implementation of these primitive mechanisms introduces concurrent argument evaluation and pattern matching, without disturbing the underlying semantics. Safe semantic extensions can be built on this foundation through the use of encapsulated types, thus avoiding the pitfalls of general operative fexprs.


References

[1]
J. Shutt. Fexprs as the basis of Lisp function application; or, $vau : the ultimate abstraction. Ph.D. Dissertation, WPI CS Department, 2010.
[2]
J. Shutt. Revised-1 Report on the Kernel Programming Language. Technical report WPI-CS-TR-05-07, Worcester Polytechnic Institute, Worcester, MA, March 2005, amended 29 October 2009.
[3]
C. Hewitt. Tutorial for ActorScriptTM extension of C#®, Java®, Objective C®, JavaScript®, and SystemVerilog using iAdaptiveTM concurrency for antiCloudTM privacy and security. ArXiv:1008.2748, 2011.
