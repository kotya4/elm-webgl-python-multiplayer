from tornado.websocket import WebSocketHandler
from serverutils import SERVER_RUNS_LOCALLY
from serverutils import SERVER_ORIGINS
import random


def bytes_to_uint16list (arr):
    return [ arr[(i << 1) + 0] | (arr[(i << 1) + 1] << 8) for i in range(len(arr) >> 1) ]


def uint16list_to_bytes (arr):
    barr = []
    for v in arr:
        barr.append((v >> 0) & 0xff)
        barr.append((v >> 8) & 0xff)
    return bytes(barr)


class Draggable:
    maxnum = 255


    def __init__ (self, id):
        self.id      = id
        self.x       = random.randint(100, 500)
        self.y       = random.randint(100, 500)
        self.offsetX = 0
        self.offsetY = 0
        self.owner   = 0


    def to_bytes (self):
        return uint16list_to_bytes([
            self.id,
            self.x,
            self.y,
            self.offsetX,
            self.offsetY,
            self.owner
        ])


    @classmethod
    def all_to_bytes (cls, draggables):
        b = bytes()
        for d in draggables:
            b = b + d.to_bytes()
        return b


class Handler (WebSocketHandler):
    clients    = set()
    draggables = [ Draggable(i) for i in range(Draggable.maxnum) ]


    def check_origin (self, origin):
        print('!!! Localhost as origin !!!')
        return origin in SERVER_ORIGINS or origin == 'http://localhost:5000'


    def open (self):
        Handler.clients.add(self)
        self.id = 0


    def on_close (self):
        # print('WebSoket connection closed')
        # print(f'Close code  : { self.close_code }')
        # print(f'Close reason: { self.close_reason }')
        Handler.clients.remove(self)
        cls = Handler
        pass

    def send_everyone (self, message):
        cls = Handler
        for client in cls.clients:
            try:
                client.write_message(bytes(message), binary=True)
            except Exception as e:
                print(e)


    def send_everyone_except_me (self, message):
        cls = Handler
        for client in cls.clients:
            if client == self:
                continue
            try:
                client.write_message(bytes(message), binary=True)
            except Exception as e:
                print(e)


    def send_only_me (self, message):
        try:
            self.write_message(bytes(message), binary=True)
        except Exception as e:
            print(e)


    @classmethod
    def get_unique_id (cls):
        """ Returns random unique integer between 1 and 255 if clients number less than 255 or returns 0 """
        client_id = 0
        is_unique = False
        i0        = random.randint(1, 0xff)
        for i in range(255):
            client_id = (i0 + i) % 255 + 1
            if not client_id in [ client.id for client in cls.clients ]:
                is_unique = True
                break
        return client_id if is_unique else 0


    def on_message (self, message):
        cls = Handler
        try:

            # print()
            # print('====================================')
            # print('recieved from client', message)

            message = bytes_to_uint16list(message)

            # init
            if   message[0] == 1:
                self.id = cls.get_unique_id()
                self.send_only_me(uint16list_to_bytes([1, self.id]) + Draggable.all_to_bytes(cls.draggables))

            # grab
            elif message[0] == 2:
                id      = message[1]
                x       = message[2]
                y       = message[3]
                offsetX = message[4]
                offsetY = message[5]
                if self.id != 0 and cls.draggables[id].owner == 0:
                    cls.draggables[id].x       = x
                    cls.draggables[id].y       = y
                    cls.draggables[id].offsetX = offsetX
                    cls.draggables[id].offsetY = offsetY
                    cls.draggables[id].owner   = self.id
                    self.send_everyone_except_me(uint16list_to_bytes([2]) + cls.draggables[id].to_bytes())
                # else:
                #     self.send_only_me(uint16list_to_bytes([2]) + cls.draggables[id].to_bytes())

            # put
            elif message[0] == 3:
                id      = message[1]
                if self.id != 0 and cls.draggables[id].owner == self.id:
                    cls.draggables[id].owner   = 0
                    self.send_everyone_except_me(uint16list_to_bytes([3]) + cls.draggables[id].to_bytes())
                # else:
                #     self.send_only_me(uint16list_to_bytes([3]) + cls.draggables[id].to_bytes())

            # move
            elif message[0] == 4:
                id      = message[1]
                x       = message[2]
                y       = message[3]
                if self.id != 0 and cls.draggables[id].owner == self.id:
                    cls.draggables[id].x = x
                    cls.draggables[id].y = y
                    self.send_everyone_except_me(uint16list_to_bytes([4]) + cls.draggables[id].to_bytes())
                # else:
                #     self.send_only_me(uint16list_to_bytes([4]) + cls.draggables[id].to_bytes())

            # err
            else:
                print(f'Incorrect action: { message[0] }, { message }')

        except Exception as e:
            print(e)
