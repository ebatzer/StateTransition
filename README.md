## Quantitative State and Transition Modelling

In environmental science, there is increasing emphasis on [resilience](https://www.sciencedirect.com/science/article/pii/S0006320714002353), the ability of an ecosystem to resist and recover from disturbance or stress. While this is an easy notion to conceptualize, it can be difficult to evaluate quantitatively -- when systems fail to adopt easily differentiated, discrete types, how do we distinguish resilience from normal variation?

In rangelands (grazed landscapes), scientists have tried to capture changes in vegetation through "State and Transition Models" (STMs). STMs describe plant dynamics as a series of discrete groups (states) that may flip to other groups over time (transitions) with some probability based on factors such as climate, soil resources, and grazing intensity. STMs offer a great way to evaluate system resilience, as states with a low transition probability can be considered particularly resistant to change. However, STMs are primarily built on the basis of expert opinion and rarely evaluated in a quantitative fashion. 

Here, I aim to complement these expert models by building an STM using a 10-year dataset of continuously observed vegetation plots. Through

__1. Identifying relevant sets of states through unsupervised clustering__

and 

__2. Modelling transitions between states as a Markov process__

I hope to provide a clearer, testable evaluation of resilience in California annual grasslands.

## Shiny App Companion

To accompany a presentation of this work at the [Society for Ecological Restoration](https://github.com/ebatzer/StateTransition/blob/master/presentations/SER%202019%20Presentation.pptx), I created a [Shiny App](https://ebatzer.shinyapps.io/stm_dynamic/) to visualize results and allow conference attendees to explore them in more detail. Scripts associated with this Shiny Appy are available under [STM_Dynamic](https://github.com/ebatzer/StateTransition/tree/master/STM_Dynamic). 

## Authors

Evan Batzer
