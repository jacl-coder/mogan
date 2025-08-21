# Elvish插件开发指南
使用Liii STEM打开`~/git/elvish/elvish.tmu`或者`$E:AppData\liiilabs\plugins\elvish\elvish.tmu`即可参与开发。

## Linux
```
cd ~/.local/share/liiilabs/plugins/
git clone git@gitee.com:LiiiLabs/elvish.git
mkdir -p ~/git; cd ~/git/
ln -s  ~/.local/share/liiilabs/plugins/elvish/ elvish
```


## Windows
```
cd $E:AppData\liiilabs\plugins
git clone git@gitee.com:LiiiLabs/elvish.git
```

## macOS
```
cd $E:HOME/Library/"Application Support"/liiilabs/plugins
git clone git@gitee.com:LiiiLabs/elvish.git
mkdir -p ~/git; cd ~/git/
ln -s  $E:HOME/Library/"Application Support"/liiilabs/plugins/elvish elvish
```