The scheduler section is a tool which houses an optimization algorithm developed in tandem with the predictive modeling efforts by FilMetrics. Glen Art Theater management can input a variety of theater constraints for a given day, such as the earliest show start time and the latest show finish time, the possible intervals between showtimes, and the available screens to schedule. Next, the film inputs provide a drop-down list of available films to be shown on the selected date, and the user may choose the minimum number of times each film must be shown that day. Upon clicking the Schedule button, an optimized schedule is generated within seconds expressly designed to increase Glen Art Theater revenues.

#### Cases when unable to meet all constraints

At the conclusion of this piloting period, the optimization algorithm is still in its infancy. There may be cases when all films will not be able to be optimally shown on the selected day during the determined screening window. This can be due to the length of the films, their forecasted demand for the day, and the amount of time available for showings. If the user encounters this error, there are a number of potential solutions.

* Lengthen the available screening window by moving up the earliest start time or moving back the latest finish time (provide more opportunities to fit in additional showings)
* Change the selection of films (consider removing the longer, less-popular films)
* Reduce the minimum required showings for each film

If these do not work, a final suggestion is to optimize the schedule for all but one of the available screens and  manually create the schedule for the final screen such that the constraint is finally met. The FilMetrics team will continue to make improvements to the algorithm in the future such that these issues are addressed and resolved.
