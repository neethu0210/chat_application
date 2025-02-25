# Erlang Chat Application

## Overview
This is a simple chat application implemented in Erlang, consisting of a **server** and multiple **clients**.

## Features

### Basic Features:
- **Configurable Server Capacity**: The server can handle up to `X` simultaneous clients (configurable).
- **Connection Limit**: If the `X+1` client tries to connect, they are rejected.
- **Broadcast Messaging**: Messages sent by any client are broadcast to all connected clients.
- **Message History**: The server keeps a log of all messages with sender names and timestamps.
- **Previous Messages on Connection**: A newly connected client receives the last `N` (configurable) messages.
- **Join/Exit Notifications**: When a client connects or disconnects, a notification is broadcast.
- **Unique Usernames**: No two clients can use the same username simultaneously.
- **Connected Clients List**: Clients can retrieve the list of currently connected users.
- **Private Messaging**: Clients can send private messages to each other.
- **Server Monitoring**: The server can list all connected clients and view the full chat history.

### Advanced Features:

#### a) Chatroom Topics
- **Change Topic**: Clients can define or update the chatroom topic.
- **Topic Broadcast**: The topic is broadcast to all users upon change.
- **Retrieve Current Topic**: Clients can request the current chatroom topic.
- **Admin-Only Topic Updates**: A configuration option restricts topic changes to admins.

#### b) Admin Controls
- **Predefined Admins**: The server can define certain users as admins.
- **Kick Users**: Admins can remove any user from the chat.
- **Mute Users**:
  - Muted users cannot send messages in the public chat.
  - Private messages from muted users are still allowed.
  - Muting can be time-based (e.g., 5 minutes) and auto-unmutes after the duration expires.
  - Admins can explicitly unmute users.
  - Muted users see a message showing when their mute expires.
- **Retrieve Admin List**: Any user can see the list of current admins.
- **Promote Users to Admin**: Admins can promote normal users to admin.

#### c) Offline Messaging
- **Private Message Storage**: If a recipient is offline, the server stores the message.
- **Delayed Delivery**: When the recipient reconnects, they receive stored messages.
- **Sender Notification**: The sender is notified if the recipient is offline and will receive the message later.
