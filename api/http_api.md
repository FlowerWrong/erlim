## http api for onechat

### NOTE: 所有api都必须带上 `token:string` 这个query params, 以下不在赘述

##### 联系人列表

* `GET` `/api/v1/contacts`

##### 黑名单列表

* `GET` `/api/v1/blocks`

##### 获取群信息, 包括群基本信息和群成员

* `GET` `/api/v1/rooms`
* `extra query params` `room_id:integer`

##### 修改群昵称

* `PUT` `/api/v1/users/room/nickname`
* `extra put params` `room_id:integer, nickname:string`

##### 修改联系人昵称

* `PUT` `/api/v1/users/friend/nickname`
* `extra put params` `friend_id:integer, nickname:string`

##### 修改群消息免打扰

* `PUT` `/api/v1/users/room/none_bother`
* `extra put params` `room_id:integer, none_bother:integer` none_bother: 0有消息通知 1无消息通知

##### 修改群聊天背景

* `PUT` `/api/v1/users/room/bg`
* `extra put params` `room_id:integer, bg:string` bg: image url

##### 举报群 TODO

##### 查找群聊天记录

* `GET` `/api/v1/rooms/chatlog`
* `extra query params` `room_id:integer, last_msg_id:integer, limit:integer`

##### 获取联系人基本信息 区分是不是好友

* `GET` `/api/v1/users`
* `extra query params` `user_id:integer`

##### 添加联系人黑名单, 无需对方同意, 无通知给对方

* `POST` `/api/v1/blocks`
* `extra post params` `friend_id:integer`

##### 移除联系人黑名单, 无需对方同意, 无通知给对方

* `DELETE` `/api/v1/blocks`
* `extra post params` `block_id:integer`


