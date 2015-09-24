## Session api

#### client send login params to server

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "login",
    "name": "yang",
    "pass": "123456",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293",
    "device": "android-xiaomi"
}
```

###### server reply to client with client send ack, ack is an uuid or others

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "login",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### Logout

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "logout"
}
```
