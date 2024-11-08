# Python

## Use conda (install conda if not installed)
```bash
conda create -n ox -c conda-forge --strict-channel-priority osmnx
conda activate ox
cd python
conda env update --file environment.yaml --prune
```

## To run python code
use conda under the environment ox
make sure that you ```conda activate ox```
```
cd map
python test_map.py
```