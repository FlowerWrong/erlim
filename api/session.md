## Session api(No ack)

#### client send login params to server

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "login",
    "name": "yang",
    "pass": "123456",
    "device": "android-xiaomi"
}
```

#### Logout

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "logout"
}
```
