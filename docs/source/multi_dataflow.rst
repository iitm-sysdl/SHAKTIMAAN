Multi-Data Flow Support
-----------------------

Overview
^^^^^^^^


Inputs to the Module
^^^^^^^^^^^^^^^^^^^^

* Weights: The weights for convolution into Weight FIFOs. Similar to what we did in weight stationary approach, but need to figure out the direction (N to S or E to W) : Need to figure out the dimensions during literature survey
* Conv. Inputs: The inputs for convolution into Input FIFOs. Similar to what we did in weight stationary approach, but need to figure out the direction (N to S or E to W) : Need to figure out the dimensions during literature survey

Outputs from the Module
^^^^^^^^^^^^^^^^^^^^^^^

* Conv. Outputs: The outputs will be sent out from Accumulation FIFOs to Accumulation buffers. Similar to what we did in weight stationary approach, but need to figure out the direction (N to S or W to E) : Need to figure out the dimensions during literature survey

Assumptions
^^^^^^^^^^^
None

Pseudo Code
^^^^^^^^^^^

* Reset Phase: For output stationary approach, no need to pass anything in initial phase, we can just reset the value stored in PE when the new computation starts.
* MAC Phase: We will be passing inputs and weights from N to S and W to E(Need to figure out which element comes from what direction). In every PE, multilpy input with weight and add it to the stored value.
* Activation and Write Phase: Once the computation is done, pass the outputs southwards/eastwards to activation function blocks. We will get one output/cycle through each of these blocks which will be enqueued in Accum FIFOs.

Milestones
^^^^^^^^^^

* 03/21: Coming up with a complete dataflow graph for output stationary algorithm, along with the dimensions of all the elements involved(inputs, weights, outputs, PE Array size, etc.)
* 04/07: Getting a complete, working code ready. 
* 04/20: Complete verification(?)

To be Resolved
^^^^^^^^^^^^^^
* Are the milestones sufficient for the assigned task, or do we need to look into other aspects as well?
* Need to discuss what would the ideal dimensions for the elements be. A clearer picture will be obtained during the literature survey phase.  
* Do we need to compare output and weight stationary approaches(to see which one is better) in this phase as well?

