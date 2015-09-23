## Chat api

#### client send one-to-one chat(must be friends) to server

```json
{
    "cmd": "single_chat",
    "to": "to user mysql id, must be integer",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server reply ack to client, and then client verify this ack

```json
{
    "cmd": "single_chat",
    "action": "single_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### server send single chat msg to client

```json
{
    "cmd": "single_chat",
    "from": "from user mysql id, must be integer",
    "msg": "Msg body",
    "ack": "mysql msg id"
}
```

###### client reply ack to server with this ack

```json
{
    "cmd": "ack",
    "action": "single_chat",
    "ack": "mysql msg id"
}
```

#### client send group chat msg to server

```json
{
    "cmd": "group_chat",
    "to": "to room mysql id, must be integer",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server reply ack to client, and then client verify this ack

```json
{
    "cmd": "ack",
    "action": "group_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### server send group chat msg to client

```json
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
{
    "cmd": "ack",
    "action": "group_chat",
    "ack": "mysql user_roommsg id, must be integer"
}
```

#### server send single chat offline msg to client

```json
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
{
    "cmd": "ack",
    "action": "offline_single_chat_msg",
    "ack": [1, 2]
}
```

#### server send group chat offline msg to client

```json
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
{
    "cmd": "ack",
    "action": "offline_group_chat_msg",
    "ack": [1, 2]
}
```


#### error

```json
{
    "cmd": "error",
    "msg": "404 Not Found user with this name",
    "code": 10404
}
```
