## A IM server written in erlang

#### Dependency

* [erlang 17.5](http://www.erlang.org/)
* [jsx](https://github.com/talentdeficit/jsx) and [jiffy](https://github.com/davisp/jiffy) for json parse
* [etoml](https://github.com/kalta/etoml) for toml parse

#### Install

```bash
make

# in erl console
application:start(erlim).
```

#### Usage

```bash
# 登陆
S1 = cy:login().
S2 = ck:login().

# 发起聊天
cy:sc().
ck:loop_recv(S2).
```

#### Design

* Use toml for config
* Use json for data exchange
* Use http for file upload

#### [API doc](https://github.com/FlowerWrong/erlim)


#### Features

* single chat: chat must be friends
* group chat: whatsapp-like
* chat history both single chat and group chat

#### 参考链接

* [Erlang 聊天室程序](http://www.cnblogs.com/yjl49/archive/2012/02/24/2371920.html)

#### Todo

- [x] erlang async receive
- [ ] one-to-one chat
- [ ] use toml to write config file
- [x] use mnesia for session store
- [x] Toml library of erlang
- [ ] yrl and xrl(语法分析/词法分析) write json parse in erlang(yecc and leex)
- [ ] How to implement pubsub with erlang(gen_event)
