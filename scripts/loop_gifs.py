
"""
GIF Looper Script

This script recursively traverses a directory to find all GIF files, checks if they are looped, and
prompts the user to replace non-looped GIFs with looped versions.

Dependencies:
- pathlib: For file system traversal
- Pillow (PIL): For image processing

"""


import os
from pathlib import Path
from PIL import Image, ImageSequence

def is_gif_looped(gif_path):
    """Check if a GIF is looped."""
    with Image.open(gif_path) as img:
        # print(img.info["loop"])
        # return True
        return img.info.get('loop', 1) == 0

def make_gif_looped(gif_path):
    """Create a looped version of a GIF."""
    with Image.open(gif_path) as img:
        frames = [frame.copy() for frame in ImageSequence.Iterator(img)]
        looped_gif_path = gif_path.with_suffix('.looped.gif')
        frames[0].save(looped_gif_path, save_all=True, append_images=frames[1:], loop=0)
    return looped_gif_path

def main():
    start_path = Path(__file__).resolve().parent.parent
    
    if not start_path.exists() or not start_path.is_dir():
        print("Invalid directory path.")
        return

    for gif_path in start_path.rglob('*.gif'):
        if "_site" in str(gif_path):
            continue
        if not is_gif_looped(gif_path):
            response = input(f"GIF {gif_path} is not looped. Do you want to replace it with a looped version? (y/n): ").strip().lower()
            if response == 'y':
                looped_gif_path = make_gif_looped(gif_path)
                os.replace(looped_gif_path, gif_path)
                
if __name__ == "__main__":
    main()
