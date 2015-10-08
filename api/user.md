## User api

#### friendship

###### 添加好友

好友请求
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "create_friendship",
    "to": "user mysql id",
    "msg": "I an yang",
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
    "notification_type": 2,
    "from": "user mysql id",
    "msg": "I an yang",
    "ack": "ack"
}
```

对方回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "notification",
    "notification_type": 2,
    "ack": "ack"
}
```

对方同意创建
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "create_friendship",
    "to": "user mysql id",
    "msg": "I an didi",
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

发送已经成为好友消息给双方
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "notification",
    "notification_type": 3,
    "from": "user mysql id",
    "ack": "ack"
}
```

客户端消息回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "notification",
    "notification_type": 3,
    "ack": "ack"
}
```

###### 删除好友(服务器端消息回执用通知替代)

删除好友请求
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "del_friendship",
    "to": "user mysql id"
}
```

发送删除好友通知给双方
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "notification",
    "notification_type": 5,
    "from": "user mysql id if you are sender, the from is 0/system message",
    "ack": "notification id"
}
```

客户端消息回执
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "notification",
    "notification_type": 5,
    "ack": "notification id"
}
```

#### blockship