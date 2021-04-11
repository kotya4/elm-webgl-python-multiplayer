from tornado.websocket import WebSocketHandler
import random
from serverutils import server_runs_locally


class Handler(WebSocketHandler):
    clients = set()

    draggable = {
        "x"       : random.randint(100, 500),
        "y"       : random.randint(100, 500),
        "owner"   : 0,
        "offsets" : 0,
    }


    def check_origin(self, origin):
        allowed_origins = [
            'https://kotya-web.herokuapp.com',
            'http://kotya-web.herokuapp.com',
            'http://www.kotya.tk',
            'http://kotya.tk',
        ]
        if server_runs_locally() or False: allowed_origins.append('http://localhost:5000')
        origin_allowed = origin in allowed_origins
        # if not origin_allowed: print(f'Origin { origin } not in { allowed_origins }')
        return origin_allowed


    def open(self):
        Handler.clients.add(self)
        self.id = 0


    def on_close(self):
        # print('WebSoket connection closed')
        # print(f'Close code  : { self.close_code }')
        # print(f'Close reason: { self.close_reason }')
        Handler.clients.remove(self)
        cls = Handler
        if cls.draggable['owner'] == self.id:
            # PutDraggable -> DraggablePutted
            cls.draggable['owner'] = 0
            self.send_everyone(cls.pack_message(0, 3, 0, cls.draggable['y'], cls.draggable['x']))


    def send_everyone(self, message):
        cls = Handler
        for client in cls.clients:
            try:
                client.write_message(bytes(message), binary=True)
            except Exception as e:
                print(e)


    def send_everyone_except_me(self, message):
        cls = Handler
        for client in cls.clients:
            if client == self:
                continue
            try:
                client.write_message(bytes(message), binary=True)
            except Exception as e:
                print(e)


    def send_only_me(self, message):
        try:
            self.write_message(bytes(message), binary=True)
        except Exception as e:
            print(e)


    @classmethod
    def get_unique_id(cls):
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


    @classmethod
    def unpack_message(cls, message):
        return {
            'action'  : message[0] | (message[1] << 8),
            'offsets' : message[2] | (message[3] << 8),
            'y'       : message[4] | (message[5] << 8),
            'x'       : message[6] | (message[7] << 8),
        }


    @classmethod
    def pack_message(cls, extra, action, owner, offsets, y, x):
        return bytes((
            action  & 0xff, extra          & 0xff,
            owner   & 0xff, 0              & 0xff,
            offsets & 0xff, (offsets >> 8) & 0xff,
            y       & 0xff, (y >> 8)       & 0xff,
            x       & 0xff, (x >> 8)       & 0xff,
        ))


    def on_message(self, message):
        cls = Handler
        try:
            # print()
            # print('====================================')
            # print('recieved from client', message)

            message = cls.unpack_message(message)

            # print('unpacked as:')
            # print('action' , message['action'] )
            # print('offsets', message['offsets'])
            # print('y'      , message['y']      )
            # print('x'      , message['x']      )

            extra   = 0
            action  = message['action']
            owner   = cls.draggable['owner']
            offsets = cls.draggable['offsets']
            y       = cls.draggable['y']
            x       = cls.draggable['x']

            # print()
            # print('draggable before action:')
            # print('owner'  , cls.draggable['owner']  )
            # print('offsets', cls.draggable['offsets'])
            # print('y'      , cls.draggable['y']      )
            # print('x'      , cls.draggable['x']      )

            # LoadDraggable -> DraggableLoaded
            if   1 == message['action']:
                extra = self.id = cls.get_unique_id()
                self.send_only_me(cls.pack_message(extra, action, owner, offsets, y, x))

            # GrabDraggable -> DraggableGrabbed
            elif 2 == message['action']:
                if self.id != 0 and (owner == 0 or owner == self.id):
                    cls.draggable['owner']   = owner   = self.id
                    cls.draggable['offsets'] = offsets = message['offsets']
                    cls.draggable['y']       = y       = message['y']
                    cls.draggable['x']       = x       = message['x']
                self.send_everyone(cls.pack_message(extra, action, owner, offsets, y, x))

            # PutDraggable -> DraggablePutted
            elif 3 == message['action']:
                if owner == self.id:
                    cls.draggable['owner']   = owner   = 0
                    cls.draggable['offsets'] = offsets = message['offsets']
                    cls.draggable['y']       = y       = message['y']
                    cls.draggable['x']       = x       = message['x']
                self.send_everyone(cls.pack_message(extra, action, owner, offsets, y, x))

            # MoveDraggable -> DraggableMoved
            elif 4 == message['action']:
                if owner == self.id:
                    cls.draggable['y'] = y = message['y']
                    cls.draggable['x'] = x = message['x']
                self.send_everyone_except_me(cls.pack_message(extra, action, owner, offsets, y, x))

            # Unknown
            else:
                print('\nBAD ACTION', message, action)

            # print()
            # print('draggable after action:')
            # print('owner'  , cls.draggable['owner']  )
            # print('offsets', cls.draggable['offsets'])
            # print('y'      , cls.draggable['y']      )
            # print('x'      , cls.draggable['x']      )

        except Exception as e:
            print(e)
