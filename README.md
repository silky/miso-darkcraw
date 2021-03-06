# Dark Craw

A card game done with Haskell's [miso](https://github.com/dmjio/miso).

You can see the current game [here](https://schplaf.org/hgames/darkcraw).

# Developers

See [app/README.md](https://github.com/smelc/miso-darkcraw/blob/master/app/README.md)
for Haskell/[miso](https://github.com/dmjio/miso/) instructions.

## Hooks

Install the pre-commit hook as follows:

`ln -sr hooks/pre-commit.py .git/hooks/pre-commit`

If you have the rights to do a release, install the pre-push hook as follows:

`ln -sr hooks/pre-push .git/hooks/pre-push`

## Assets

There are two kind of assets at the moment:

* `app/assets/16x16*.png` and `app/assets/24x24*.png`. Generate them
  by executing `./scripts/GenAssets.hs` whenever `assets/16x16.png`
  or `assets/24x24.png` change.
* `app/assets/*.png`. [@smelc](https://github.com/smelc) generates them from:

  * `tiled/*.tmx` using [tiled](https://www.mapeditor.org/)
  * `xcf/*.xcf` using [gimp](https://www.gimp.org/)

  Execute `./scripts/dl-large-assets.sh` to download up-to-date versions.

