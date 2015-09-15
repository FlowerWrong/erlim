## An IM server written in erlang

#### Dependency

* [erlang 17.5](http://www.erlang.org/) +
* [jsx](https://github.com/talentdeficit/jsx) and [jiffy](https://github.com/davisp/jiffy) for json parse
* [etoml](https://github.com/kalta/etoml) for toml parse
* [erlang-uuid-v4](https://github.com/afiskon/erlang-uuid-v4) for token generate
* [emysql](https://github.com/Eonblast/Emysql) for mysql driver

#### Install

```bash
make

# in erl console
application:start(erlim).
```

#### Usage

```erlang
```

#### Design

* Just for chat(both one2one and group chat), no api for add_friend/del_friend/create_room/upload_file/del_room etc...
* Use toml for config
* Use json for data exchange

![structure of erlim](https://raw.githubusercontent.com/FlowerWrong/erlim/master/api/erlim_structure.png)

#### [API doc](https://github.com/FlowerWrong/erlim/tree/master/api)


#### Features

* single chat: chat must be friends
* group chat: whatsapp-like
* chat history both single chat and group chat

#### Todo

- [x] erlang async receive
- [x] one-to-one chat
- [x] group chat
- [x] use toml to write config file
- [x] use mnesia for session store
- [x] toml library of erlang
- [x] handle bug and error
- [x] chat must be friends
- [x] group chat must in this room
- [x] remove token 不是无状态的
- [ ] add ACK(消息回执)
- [ ] keep session(避免移动端重连风暴)
- [ ] yrl and xrl(语法分析/词法分析) write json parse in erlang(yecc and leex)
- [ ] implement pubsub with erlang(gen_event)
- [ ] implement cluster mode
