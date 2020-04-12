### Building the Docs

#### Install Python dependencies

```
pip intall -r requirements.txt
```

#### To build the pdf:

```
make latexpdf
evince build/latex/dnn_accelerator.pdf &

```

#### Top build html:

```
make html
firefox build/html/index.html
```
