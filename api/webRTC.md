## webRTC api(websocket)

#### client send want to video chat with other, it send a `webrtc_create` to server

```json
{
    "cmd": "webrtc_create",
    "to": "to room mysql id, must be integer",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server send request to other peer

```json
{
    "cmd": "webrtc_create",
    "from": "from user mysql id, must be integer",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server reply ack to client, and then client verify this ack

```json
{
    "cmd": "ack",
    "action": "webrtc_create",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```


#### other peer send `webrtc_join` to server

```json
{
    "cmd": "webrtc_join",
    "to": "to room pg2 id, must be integer",
    "ack": "to room pg2 id, must be integer"
}
```

###### server send peer's `webrtc_join` to room members

```json
{
    "cmd": "webrtc_join",
    "from": "from user mysql id, must be integer",
    "to": "to room pg2 id, must be integer",
    "ack": "to room pg2 id, must be integer"
}
```

###### server reply ack to client, and then client verify this ack

```json
{
    "cmd": "ack",
    "action": "webrtc_join",
    "ack": "to room pg2 id, must be integer"
}
```


#### peer send `webrtc_leave` to server

```json
{
    "cmd": "webrtc_leave",
    "to": "to room pg2 id, must be integer",
    "ack": "to room pg2 id, must be integer"
}
```

###### server send peer's `webrtc_leave` to room members

```json
{
    "cmd": "webrtc_leave",
    "from": "from user mysql id, must be integer",
    "to": "to room pg2 id, must be integer",
    "ack": "to room pg2 id, must be integer"
}
```

###### server reply ack to client, and then client verify this ack

```json
{
    "cmd": "ack",
    "action": "webrtc_leave",
    "ack": "to room pg2 id, must be integer"
}
```

#### [信令](http://segmentfault.com/a/1190000000439103)

###### client send a `webrtc_send_offer` to server

```json
{
    "cmd": "webrtc_send_offer",
    "to": "to mysql user id, must be integer",
    "sdp": "session_description.sdp",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server send this offer to peer

```json
{
    "cmd": "webrtc_send_offer",
    "from": "from mysql user id, must be integer",
    "sdp": "session_description.sdp",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### peer reply a answer to server

```json
{
    "cmd": "webrtc_send_answer",
    "to": "to mysql user id, must be integer",
    "sdp": "session_description.sdp",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server send this answer to client

```json
{
    "cmd": "webrtc_send_answer",
    "from": "from mysql user id, must be integer",
    "sdp": "session_description.sdp",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```


#### [ICE候选 交换网络信息](http://www.cnblogs.com/lingyunhu/p/4058182.html)

###### client send `webrtc_send_ice_candidate` to server

```json
{
    "cmd": "webrtc_send_ice_candidate",
    "to": "to mysql user id, must be integer",
    "label": "event.candidate.sdpMLineIndex",
    "candidate": "event.candidate.candidate",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
}
```

###### server send this `webrtc_send_ice_candidate` to peer

```json
{
    "cmd": "webrtc_send_ice_candidate",
    "from": "from mysql user id, must be integer",
    "label": "event.candidate.sdpMLineIndex",
    "candidate": "event.candidate.candidate",
    "ack": "72cdf1ae-62a3-4ebf-821c-a809d1931293"
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
