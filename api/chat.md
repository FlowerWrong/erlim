## Chat api

#### client send one-to-one chat(must be friends) to server

```json
{
    "cmd": "single_chat",
    "to": "to user name",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server reply ack to client, and then client verify this ack

```json
{
    "cmd": "ack",
    "action": "single_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### server send single chat msg to client

```json
{
    "cmd": "ack",
    "action": "single_chat",
    "ack": 1  // mysql msg id
}
```

###### client reply ack to server with this ack

```json
{
    "cmd": "ack",
    "action": "single_chat",
    "ack": 1
}
```

#### client send group chat msg to server

```json
{
    "cmd": "group_chat",
    "to": "to room mysql id",
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
    "cmd": "ack",
    "action": "group_chat",
    "ack": 1  // mysql user_roommsg id
}
```

###### client reply ack to server with this ack

```json
{
    "cmd": "ack",
    "action": "group_chat",
    "ack": 1
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