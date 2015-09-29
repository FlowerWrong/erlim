## User api

#### friendship

###### 添加好友

好友请求
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "create_friendship",
    "to": "user mysql id",
    "ack": "ack"
}
```

消息回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "create_friendship",
    "ack": "ack"
}
```

发送好友请求给对方
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "notification",
    "from": "user mysql id",
    "ack": "ack"
}
```

对方回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "create_friendship",
    "ack": "ack"
}
```

对方统一创建
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "create_friendship",
    "to": "user mysql id",
    "ack": "ack"
}
```

消息回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "create_friendship",
    "ack": "ack"
}
```

发送已经成为好友消息
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "become_friends",
    "from": "user mysql id",
    "ack": "ack"
}
```

消息回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "become_friends",
    "ack": "ack"
}
```

#### blockship