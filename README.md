## Introduction

*The Iron Council* - A Lone Gunship Aginst the Enemy

Click on the image to see the latest video showing a basic track rendering.

[![Recent Video](https://img.youtube.com/vi/aaYTu9KVmBg/hqdefault.jpg)](https://www.youtube.com/watch?v=aaYTu9KVmBg)

## Features

  * Player controlled gunship, with yaw movement (either in the direction of left/right movement, or opposite it). **Done**
  * Battle against a massive, heavily fortified train. And maybe more.
  * BulletML patterns for enemy bullets.
  * Choice of 'option' - auxiliary weapon: extra cannons or rockets **Done** (press 'o' before '1' to switch option types)
  * *Some* type of procedurally generated terrain.

## How to try it out?

Clone the repository and build it locally with Lein. You'll also need my [pixel-ships](https://github.com/the2bears/pixel-ships) library and my fork of [play-clj](https://github.com/the2bears/play-clj) (Why? The Box2D shapes won't overlap correctly with the textures during rotation without my patch).

Build and install these locally. Press '1' at blank screen to launch the gunship.


## License

Copyright © 2016-2017 William Swaney

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
