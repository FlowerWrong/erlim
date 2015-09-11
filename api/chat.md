## Chat api

#### one-to-one chat(must be friends)

```json
{
    "cmd": "single_chat",
    "token": "efd0c3b8-35bb-4cd1-85db-9248d45d8afa",
    "to": "to user name",
    "msg": "msg body"
}
```

#### Group chat

```json
{
    "cmd": "group_chat",
    "token": "efd0c3b8-35bb-4cd1-85db-9248d45d8afa",
    "to": "to room mysql id",
    "msg": "msg body"
}
```
