## Chat api

#### one-to-one chat(must be friends)

```json
{
    "cmd": "single_chat",
    "to": "to user name",
    "msg": "msg body"
}
```

#### Group chat

```json
{
    "cmd": "group_chat",
    "to": "to room mysql id",
    "msg": "msg body"
}
```
