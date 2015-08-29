## A IM server written in erlang

#### Dependency

* [erlang 17.5](http://www.erlang.org/)
* [toml](https://github.com/toml-lang/toml)
* [jsx for json](https://github.com/talentdeficit/jsx)
* [etoml for toml](https://github.com/kalta/etoml)

#### Install

#### Usage

#### Design

* Use toml for config
* Use json for data exchange
* Use http for file upload

###### register/login

```json
{
    "cmd": "register/login",
    "username": "yang",
    "password": "123456"
}
```

###### one-to-one/group chat

```json
{
    "cmd": "single_chat/group_chat",
    "from": "from user/room name",
    "to": "to user name",
    "msg": "msg body",
    "token": "token hash"
}
```

###### logout

```json
{
    "cmd": "logout",
    "username": "yang",
    "token": "token hash"
}
```

#### Features

* single chat: chat must be friends
* group chat: whatsapp-like
* chat history both single chat and group chat

#### 参考链接

* [Erlang 聊天室程序](http://www.cnblogs.com/yjl49/archive/2012/02/24/2371920.html)

#### Todo

- [ ] one-to-one chat
- [ ] use mnesia for session store
- [ ] use redis for token store
- [x] Toml library of erlang
- [ ] yrl and xrl(语法分析/词法分析) write json parse in erlang(yecc and
  leex)
