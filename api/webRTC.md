## webRTC api(websocket) without ack

#### client send want to video chat with other, it send a `webrtc_create` to server

```json
{
    "cmd": "webrtc_create",
    "to": "to user mysql id, must be integer",
    "name": "room name"
}
```

###### server send request to other peer

```json
{
    "cmd": "webrtc_create",
    "from": "from user mysql id, must be integer",
    "room_uuid": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server send room msg to client

```json
{
    "cmd": "webrtc_create",
    "from": "from user mysql id, must be integer, 此处是自己的id",
    "room_uuid": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```


#### other peer send `webrtc_join` to server

```json
{
    "cmd": "webrtc_join",
    "to": "to room uuid, must be string"
}
```

###### server send peer's `webrtc_join` to room members

```json
{
    "cmd": "webrtc_join",
    "from": "from user mysql id, must be integer",
    "to": "to room uuid, must be integer"
}
```

#### peer send `webrtc_leave` to server

```json
{
    "cmd": "webrtc_leave",
    "to": "to room uuid, must be integer"
}
```

###### server send peer's `webrtc_leave` to room members

```json
{
    "cmd": "webrtc_leave",
    "from": "from user mysql id, must be integer",
    "to": "to room uuid, must be integer"
}
```

#### [信令](http://segmentfault.com/a/1190000000439103)

###### client send a `webrtc_send_offer` to server

```json
{
    "cmd": "webrtc_send_offer",
    "to": "to mysql user id, must be integer",
    "sdp": "session_description.sdp"
}
```

###### server send this offer to peer

```json
{
    "cmd": "webrtc_send_offer",
    "from": "from mysql user id, must be integer",
    "sdp": "session_description.sdp"
}
```

###### peer reply a answer to server

```json
{
    "cmd": "webrtc_send_answer",
    "to": "to mysql user id, must be integer",
    "sdp": "session_description.sdp"
}
```

###### server send this answer to client

```json
{
    "cmd": "webrtc_send_answer",
    "from": "from mysql user id, must be integer",
    "sdp": "session_description.sdp"
}
```


#### [ICE候选 交换网络信息](http://www.cnblogs.com/lingyunhu/p/4058182.html)

###### client send `webrtc_send_ice_candidate` to server

```json
{
    "cmd": "webrtc_send_ice_candidate",
    "to": "to mysql user id, must be integer",
    "label": "event.candidate.sdpMLineIndex",
    "candidate": "event.candidate.candidate"
}
```

###### server send this `webrtc_send_ice_candidate` to peer

```json
{
    "cmd": "webrtc_send_ice_candidate",
    "from": "from mysql user id, must be integer",
    "label": "event.candidate.sdpMLineIndex",
    "candidate": "event.candidate.candidate"
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
