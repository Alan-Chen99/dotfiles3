## Notes to self

```bash
# https://superuser.com/questions/1586386/how-to-find-wsl2-machines-ip-address-from-windows
wsl hostname -I

# https://jwstanly.com/blog/article/Port+Forwarding+WSL+2+to+Your+LAN/
netsh interface portproxy add v4tov4 listenport=[PORT] listenaddress=0.0.0.0 connectport=[PORT] connectaddress=[WSL_IP]

# ssh
netsh interface portproxy add v4tov4 listenport=2222 listenaddress=0.0.0.0 connectport=22 connectaddress=172.22.118.103

netsh interface portproxy add v4tov4 listenport=5201 listenaddress=0.0.0.0 connectport=5201 connectaddress=172.22.118.103
```

```bash
# https://superuser.com/questions/1606213/how-do-i-get-back-unused-disk-space-from-ubuntu-on-wsl2

# https://github.com/microsoft/WSL/issues/4699#issuecomment-660104214
wsl --shutdown
wsl -l -v
wsl --export <DistroName> <PathToTarArchive>
wsl --unregister <DistroName>
wsl --import <DistroName> <PathToDistroNewDirectory> <PathToTarArchive>
wsl -l -v

# https://superuser.com/a/1734392
diskpart /s .\wsl-compact-disk.txt
```

```
https://github.com/microsoft/wslg/wiki/Controlling-WSLg-frame-rate
https://github.com/microsoft/wslg/issues/1142#issuecomment-1810089480

# put in:
%USERPROFILE%\.wslgconfig
```
