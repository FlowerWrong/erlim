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