# README #
Systolic Array Architectures.	

Documentation:[PDF](https://gitlab.com/shaktiproject/haml/systolic_array/-/jobs/artifacts/docs/raw/dnn_accelerator.pdf?job=docs)

## Verification
- `verify_array.bsv` - created a MxN systolic array, with `mulWidth` parameterizable. Status: Works perfectly, even after introducing random stalls.
- `verify_with_buffers.bsv` - verified mksystolic module in `systolic.bsv` file. Status: Works perfectly, even after introducing random stalls.
