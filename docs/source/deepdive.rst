################
Design Deep Dive
################

This section will discuss about the current development and the status of
implementation of various blocks shown in :numref:`microarch_deep`

.. _microarch_deep:

.. figure:: images/microarchitecture.png
   :align: center

   Micro-Architecture of the DNN Accelerator

.. include:: address_gen_unit.rst
.. include:: dependency_resolver.rst
.. include:: fetch.rst
.. include:: multi_dataflow.rst
.. include:: tensor_alu.rst
.. include:: col2im.rst
.. include:: im2col.rst
.. include:: tensor_alu.rst

