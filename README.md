# π Circuit

This Fortran/Python project uses Monte Carlo sampling to find an electrical
circuit composed of 1Ω resistors with the total resistance as close to π Ω as
possible.

The results are presented in a [Jupyter notebook](https://github.com/azag0/pi-circuit/blob/master/tests.ipynb).

To reproduce (requires Python 3, Jupyter Notebook, and a Fortran compiler):

```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
make test
make f2py
python -m ipykernel install --user --name=pi-circuit
jupyter notebook tests.ipynb  # and rerun the notebok
```
