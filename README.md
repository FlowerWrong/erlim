## An IM server written in erlang

#### Dependency

* [erlang 17.5](http://www.erlang.org/) +
* [jsx](https://github.com/talentdeficit/jsx) and [jiffy](https://github.com/davisp/jiffy) for json parse
* [etoml](https://github.com/kalta/etoml) for toml parse
* [erlang-uuid-v4](https://github.com/afiskon/erlang-uuid-v4) for token generate
* [emysql](https://github.com/Eonblast/Emysql) for mysql driver

#### Install

```bash
rebar xref

make
```

#### Usage

```erlang
make release
cp erlim.example.toml /etc/erlim/erlim.toml

./rel/erlim/bin/erlim start
./rel/erlim/bin/erlim console
./rel/erlim/bin/erlim attach
./rel/erlim/bin/erlim stop

./rel/erlim/bin/erlim_ctl status
./rel/erlim/bin/erlim_ctl vm all | memory | process
./rel/erlim/bin/erlim_ctl cluster
./rel/erlim/bin/erlim_ctl sessions
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
* websocket support
* ONECHAT protocol(Design by FlowerWrong) support

#### Cluster

* [A_Framework_for_Clustering_Generic_Server_Instances](https://erlangcentral.org/wiki/index.php?title=A_Framework_for_Clustering_Generic_Server_Instances)
* [ejabberd cluster](https://raymii.org/s/tutorials/Set_up_a_federated_XMPP_Chat_Network_with_ejabberd.html)
* [load balance long tcp connection](http://stackoverflow.com/questions/8915959/how-do-you-load-balance-tcp-traffic)
* [mnesia cluster](http://stackoverflow.com/questions/787755/how-to-add-a-node-to-an-mnesia-cluster)

#### VOIP

* [Open+Source+VOIP+Software](http://www.voip-info.org/wiki/view/Open+Source+VOIP+Software)
* [nksip](https://github.com/kalta/nksip)
* [使用Mini Sipserver 搭建小型的sip服务器](http://blog.csdn.net/cazicaquw/article/details/7345327)
* [asterisk](http://www.asterisk.org/downloads/source-code)
* [freeswitch](https://freeswitch.org/)

#### webRTC

* [talky](https://talky.io/)
* [simplewebrtc](http://simplewebrtc.com/)
* [easyrtc](https://easyrtc.com)
* [STUN and TURN server in c](https://github.com/otalk/restund/tree/master/docs)
* [stun server in c++](http://www.stunprotocol.org/)
* [BitTorrent over WebRTC](https://github.com/feross/webtorrent/)
* [erlang webrtc-sig-server](https://github.com/mlodzianck/webrtc-sig-server)
* [nodejs webrtc signaling server](https://github.com/LingyuCoder/SkyRTC)
* [webrtcexample](https://github.com/fycth/webrtcexample)
* [Building a signaling server in Erlang](https://www.packtpub.com/packtlib/book/Application-Development/9781783284450/1/ch01lvl1sec09/Building%20a%20signaling%20server%20in%20Erlang)
* [erlang Socket acceptor pool for TCP protocols.](https://github.com/ninenines/ranch)
* [peerjs](http://peerjs.com/)
* [rtc](http://rtc.io/)
* Session Description Protocol(SDP)
* [webRTC.io-erlang](https://github.com/cavedweller/webRTC.io-erlang/blob/master/src/webRTCio_server.erl)
* [webRTC.io-client](https://github.com/webRTC-io/webrtc.io-client/blob/master/lib/webrtc.io.js)
* [js sdp](http://fisheye.igniterealtime.org/browse/openfire/trunk/src/plugins/jitsivideobridge/src/js/webrtc.sdp.js?r=13852)
* [mozilla developer api](https://developer.mozilla.org/en-US/docs/Web/API/RTCSessionDescription/sdp)
* [signaling and ice](http://segmentfault.com/a/1190000000439103)
* [WebRTC-Experiment](https://github.com/muaz-khan/WebRTC-Experiment)

#### Todo

- [x] erlang async receive
- [x] one-to-one chat must be friends
- [x] group chat
- [x] use toml to write config file
- [x] use mnesia for session store
- [x] remove token
- [x] add ACK
- [x] multi device support
- [x] add mnesia cluster support
- [x] add websocket support
- [x] [implement a module like pg2 with mnesia](https://github.com/erlang/otp/blob/maint/lib/kernel/src/pg2.erl)
- [x] add video chat support(ICE  STUN: RFC3489  TURN: RFC5766  SIP  WebRTC SDP)
- [x] implement cluster mode with a scheduler in erlang or nodejs or ruby on rails
- [x] block user, add friends, room onechat api
- [x] add tls support
- [x] add edoc
- [x] http api module(friends list, blocks list, room info...)
- [ ] timeout
- [ ] add max connection number test
- [ ] add web admin support
- [ ] multi video chat without p2p, with rtmp and hls
- [ ] keep session
- [ ] implement pubsub with erlang(gen_event)
- [ ] yrl and xrl write json parse in erlang(yecc and leex)
