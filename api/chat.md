## Chat api

#### one-to-one chat(must be friends)

```json
{
    "cmd": "single_chat",
    "to": "to user name",
    "msg": "msg body",
    "t": 1442289073960718
}
```

```json
{
    "cmd": "ack",
    "action": "single_chat",
    "t": 1442289073960718
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

```json
{
    "cmd": "ack",
    "action": "group_chat",
    "t": 1442289073960718
}
```
