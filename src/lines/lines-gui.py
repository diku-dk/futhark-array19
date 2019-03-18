#!/usr/bin/env python
#
# Simple frontend for the lines program. The frontend uses sdl2 for
# visualisation.  If your Python is healthy,
# 'pip install --user imageio pypng pysdl2'
# should be sufficient.

from lines import lines

import numpy as np
from sdl2 import *
import sdl2.ext
import time
import sys

lines = lines(interactive=True)

width=1024
height=768

size=(width, height)
SDL_Init(SDL_INIT_EVERYTHING)
window = SDL_CreateWindow("Lines 2D",
                          SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,
		          width, height, SDL_WINDOW_SHOWN)

## Called initially and when switching between full screen and windowed mode.
def reWindow(window):
    window_surface = SDL_GetWindowSurface(window)
    frame_py = np.ndarray(shape=(height, width), dtype=np.int32, order='C')
    surface = SDL_CreateRGBSurfaceFrom(frame_py.ctypes.data, width, height, 32, width*4,
                                       0xFF0000, 0xFF00, 0xFF, 0x00000000)
    return (window_surface, frame_py, surface)

(window_surface, frame_py, surface) = reWindow(window)

# font = pygame.font.Font(None, 36)

def showText(what, where):
    return
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

state = lines.start_state(1337, height, width)
velocity_x = 0
velocity_y = 0
samplerate = 0.025 # very high

def render():
    global state
    futhark_start = time.time()

    state = lines.step(velocity_x,velocity_y, state)
    frame_fut = lines.render(state)
    frame_fut.get(ary=frame_py)

    futhark_end = time.time()

    SDL_BlitSurface(surface, None, window_surface, None)
    SDL_UpdateWindowSurface(window)

    print("")
    print("Futhark calls took %.2fms" % ((futhark_end-futhark_start)*1000))
    print("Velocity_x: %f (left/right to change)" % velocity_x)
    print("Velocity_y: %f (up/down to change)" % velocity_y)
    print("\033[F" * 5)

# pygame.key.set_repeat(500, 50)

running=True

while running:
    render()

    events = sdl2.ext.get_events()
    for event in events:
        if event.type == SDL_QUIT:
            running=False
        if event.type == SDL_KEYDOWN:
            key = event.key.keysym.sym
            if key == SDLK_UP:
                velocity_y -= 1
            elif key == SDLK_DOWN:
                velocity_y += 1
            elif key == SDLK_LEFT:
                velocity_x -= 1
            elif key == SDLK_RIGHT:
                velocity_x += 1
            elif key == SDLK_q:
                running = False
            elif key == SDLK_f:
                SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN)
                (window_surface, frame_py, surface) = reWindow(window)
            elif key == SDLK_g:
                SDL_SetWindowFullscreen(window, 0)
