# chatroom

使用erlang实现的聊天室

## 启动server

``` bash
./rebar3 shell --apps chatroom
```

## 启动client及使用

启动

``` bash
./rebar3 shell --apps chatroom
```

设置名称

``` erlang
connection:login(UserName) %% UserName为字符串类型
```

发送聊天

``` erlang
connection:send(Chat) %% Chat为字符串类型
```