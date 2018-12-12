# π Circuit

This Fortran/Python project uses Monte Carlo sampling to find an electrical
circuit composed of 1Ω resistors with the total resistance as close to π Ω as
possible.

Requires Python 3, Jupyter Notebook, and a Fortran compiler.

To reproduce:

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
make test
make f2py
python -m ipykernel install --user --name=pi-circuit
jupyter notebook tests.ipynb  # and rerun the notebok
```
