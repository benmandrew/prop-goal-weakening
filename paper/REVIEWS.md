# Reviews

## Review 1

> **2**: accept

The paper presents a brief outline of the author's intended Ph.D. research and the part of it that has already been completed. Overall, given a system specification and some desirable properties the objective is to find a weakened set of properties such that (1) a modification of the system specification capturing some form of error still satisfies the new set of properties, and (2) the weakened properties are still stronger than minimal requirements.

The paper briefly reports the work that has been done, in which the specification, the desirable properties and also the minimal requirements are modelled by propositional formulae. A SAT solver is exploited to find counterexamples for error cases, which are then synthesized to obtain a solution to the problem. While this is only sketched and no proof of correctness is given, it is not hard to see that the approach is feasible, though with little usability due to the restriction to propositional logic.

The author then briefly describes planned extensions exploring finite automata, Büchi automata and ASMs. This will be much more interesting and challenging.

The work is acceptable for the doctoral symposium, but in any case the approach to the planned extensions should be more elaborated.

## Review 2

> **1**: weak accept

This paper describes research objectives aimed at developing a systematic method for weakening logical specifications in response to counterexamples. Specifically, the objectives include identifying when a specification is too strong relative to a given system's behavior and refining it accordingly while maintaining essential correctness properties. The authors propose an initial approach based on counterexample-guided weakening, where counterexamples to a specification are used to iteratively relax constraints in the logical formula.

So far, this initial approach consists of applying counterexample-guided refinement techniques in the context of propositional logic. The method uses a SAT solver to identify counterexamples to a specification, which then informs an iterative process of weakening the formula to exclude these counterexamples. This approach is demonstrated through a simple toy example, as the inherent expressiveness limitations of propositional logic do not allow for much more complexity.

Clearly, this initial approach based on finding counterexamples will not scale well to more expressive logics and complex systems. The described algorithm has exponential complexity in the number of propositional variables, making it infeasible for large-scale applications. Moreover, given that the problem is NP-complete, practical scalability is a significant concern.

One relevant topic that could provide an alternative perspective on weakening specifications is partial or graded logics. Many-valued logics and fuzzy logic introduce notions of approximate satisfaction of a specification. These frameworks allow for degrees of correctness rather than a strict binary evaluation, which may align well with the goal of finding appropriately weakened specifications.

On page 3, the authors state that propositional formulae are considered to be partially ordered by implication. However, this is not always the case, and further elaboration on the limitations of this assumption would be beneficial. For instance, logical equivalences may introduce ambiguities that challenge the notion of a strict partial order.

Additionally, the claim that the algorithm is complete for systems with a finite number of variables requires clarification. While a SAT solver can determine satisfiability, it does not guarantee a feasible solution in a reasonable time frame for large or complex instances. As such, the algorithm may be "complete" in a theoretical sense, but not always computationally feasible.

The methods the authors mention as ongoing explorations, such as automata learning and interpolation, might indeed be better suited for handling more complex specifications. The current approach, while serving as an illustrative proof of concept, will not scale to real-world problems. An future direction that could be considered is whether LLMs could assist in searching for weaker properties which can then be validated and/or refined by automated means.

All in all, this paper presents a valid short contribution to the PhD symposium. While the initial approach is limited in scalability, it opens the door to valuable discussions on alternative methods that could be explored to achieve similar goals more efficiently.

## Review 3

> **2**: accept

This doctoral symposium paper presents a PhD project addressing two main research questions 1) How to automatically weaken system properties when they are invalidated by system degradation or external conditions change, 2) how this weakening affects the assume-guarantee reasoning in composed systems.

The paper is easy to follow, motivates the research, and shows a first algorithm that iteratively wakens a property until it is satisfied in a system model.

The relation of this project to state-based formalism is only addressed in future work. In any case, the project is interesting, and the paper can be discussed in the Doctoral symposium section of the conference.


