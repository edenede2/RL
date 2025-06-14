from pathlib import Path
import shutil
from importlib import import_module

setup = import_module('RL_Maggie.python.00_setup')
RAW_SHARED_DIR = setup.RAW_SHARED_DIR
LOCAL_DATA_DIR = setup.LOCAL_DATA_DIR


def copy_files_from_rl(source_dir: Path, dest_dir: Path) -> None:
    dest_dir.mkdir(parents=True, exist_ok=True)
    subdirs = list(source_dir.rglob('*'))
    rl_subdirs = [d for d in subdirs if d.is_dir() and d.name == 'RL']

    for subdir in rl_subdirs:
        for file in subdir.iterdir():
            if file.is_file() and file.name.startswith('sub_') and file.name.endswith('.csv'):
                shutil.copy2(file, dest_dir / file.name)


if __name__ == '__main__':
    copy_files_from_rl(RAW_SHARED_DIR, LOCAL_DATA_DIR)
