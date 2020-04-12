############
Introduction
############

The intent of this document is to give the full-picture view of the ongoing DNN accelerator design using systolic arrays. In this document, we take a bottom-up approach where we start with the smallest primitive component of the hardware and build it one step at a time and end with the full-system view. We will try to address all the design decisions and try to comment on how each decision affects the overall system performance (although it's easier to address this if we give a top-down view - but from a design point of view I felt bottom-up would help). This document is still in its nascent stages and will evolve overtime. 
