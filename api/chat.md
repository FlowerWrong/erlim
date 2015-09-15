## Chat api

#### one-to-one chat(must be friends)

```json
{
    "cmd": "single_chat",
    "to": "to user name",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

```json
{
    "cmd": "ack",
    "action": "single_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

#### Group chat

```json
{
    "cmd": "group_chat",
    "to": "to room mysql id",
    "msg": "msg body",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

```json
{
    "cmd": "ack",
    "action": "group_chat",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```
