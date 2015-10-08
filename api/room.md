## Room api(群)

#### create room(name must send)

```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "create_room",
    "name": "room name",
    "invitable": "0/1",
    "members": [1, 2, 3, 5, 78],
    "password": "password",
    "description": "description",
    "subject": "subject",
    "logo": "logo url"
}
```

server send notification to all(include creator)
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "notification",
    "notification_type": 11,
    "from": "user mysql id",
    "msg": "I an yang",
    "ack": "notification mysql id"
}
```

client send the ack to server to ack this notification
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "notification",
    "notification_type": 11,
    "ack": "notification mysql id"
}
```

#### del room

```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "del_room",
    "roomid": "room mysql id"
}
```

server send notification to all(include creator)
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "notification",
    "notification_type": 12,
    "from": "user mysql id",
    "msg": "I an yang",
    "ack": "notification mysql id"
}
```

client send the ack to server to ack this notification
```
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "notification",
    "notification_type": 12,
    "ack": "notification mysql id"
}
```