## Chat api

#### client send one-to-one chat(must be friends) to server

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "single_chat",
    "to": "to user mysql id, must be integer",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server reply ack to client, and then client verify this ack

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "single_chat",
    "action": "single_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### server send single chat msg to client

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "single_chat",
    "from": "from user mysql id, must be integer",
    "msg": "Msg body",
    "ack": "mysql msg id"
}
```

###### client reply ack to server with this ack

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "single_chat",
    "ack": "mysql msg id"
}
```

#### client send group chat msg to server

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "group_chat",
    "to": "to room mysql id, must be integer",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server reply ack to client, and then client verify this ack

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "group_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### server send group chat msg to client

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "group_chat",
    "from": "from user mysql id, must be integer",
    "to": "to room id",
    "msg": "Msg body",
    "ack": "mysql user_roommsg id, must be integer"
}
```

###### client reply ack to server with this ack

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "group_chat",
    "ack": "mysql user_roommsg id, must be integer"
}
```

#### server send single chat offline msg to client

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "offline_single_chat_msg",
    "msg": [
        {
            "id": 1,
            "f": 1,
            "t": 2,
            "msg": "hello world",
            "unread": 1,
            "created_at": 1442855559,
            "updated_at": 1442855559
        },
        {
            "id": 2,
            "f": 1,
            "t": 2,
            "msg": "hello world",
            "unread": 1,
            "created_at": 1442855559,
            "updated_at": 1442855559
        }
    ],
    "ack": [1, 2]
}
```

###### client reply ack to server with this ack

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "offline_single_chat_msg",
    "ack": [1, 2]
}
```

#### server send group chat offline msg to client

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "offline_group_chat_msg",
    "msg": [
        {
            "id": 1,
            "f": 1,
            "t": 2,
            "msg": "hello world",
            "created_at": 1442855559,
            "updated_at": 1442855559
        },
        {
            "id": 2,
            "f": 1,
            "t": 2,
            "msg": "hello world",
            "created_at": 1442855559,
            "updated_at": 1442855559
        }
    ],
    "ack": [1, 2]
}
```

###### client reply ack to server with this ack

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "ack",
    "action": "offline_group_chat_msg",
    "ack": [1, 2]
}
```


#### error

```json
ONECHAT/1.0\r\nPAYLOAD_LEN: 587\r\n\r\n
{
    "cmd": "error",
    "msg": "404 Not Found user with this name",
    "code": 10404
}
```
