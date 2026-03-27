import os

def rename_files(root_dir='.'):
    is_renamed = False
    for dirpath, dirnames, filenames in os.walk(root_dir):
        for filename in filenames:
            if ' ' in filename:
                old_path = os.path.join(dirpath, filename)
                new_filename = filename.replace(' ', '_')
                new_path = os.path.join(dirpath, new_filename)
                os.rename(old_path, new_path)
                print(f'Renamed: "{old_path}" → "{new_path}"')
                is_renamed = True

    if not is_renamed: print('Renamed nothing.')

if __name__ == '__main__':
    # rename_files('img')
    rename_files()
