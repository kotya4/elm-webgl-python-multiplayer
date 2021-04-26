from tornado.websocket import WebSocketHandler
from serverutils import SERVER_RUNS_LOCALLY, SERVER_ORIGINS
import random
import chessdb
import time


def bytes_to_uint16list (arr):
    return [ arr[(i << 1) + 0] | (arr[(i << 1) + 1] << 8) for i in range(len(arr) >> 1) ]


def uint16list_to_bytes (arr):
    barr = []
    for v in arr:
        barr.append((v >> 0) & 0xff)
        barr.append((v >> 8) & 0xff)
    return bytes(barr)


def uint32list_to_bytes (arr):
    barr = []
    for v in arr:
        barr.append((v >> 0x00) & 0xff)
        barr.append((v >> 0x08) & 0xff)
        barr.append((v >> 0x10) & 0xff)
        barr.append((v >> 0x18) & 0xff)
    return bytes(barr)


def attributes (o):
    return [ item for item in vars(o).items() if not item[0].startswith('__') ]


def test_uniqueness (values):
    for v1 in values:
        equalnum = 0
        for v2 in values:
            if v1 == v2:
                equalnum += 1
        if equalnum > 1:
            return False
    return True


class DraggableType:
    WHITE_QUEEN  = ( 0x01,  1, (  10, 100 ) )
    WHITE_PAWN   = ( 0x02,  8, (  10, 200 ) )
    WHITE_KNIGHT = ( 0x03,  2, (  10, 300 ) )
    WHITE_ROOK   = ( 0x04,  2, (  10, 400 ) )
    WHITE_BISHOP = ( 0x05,  2, (  10, 500 ) )
    WHITE_KING   = ( 0x06,  1, (  10, 600 ) )
    BLACK_QUEEN  = ( 0x07,  1, ( 920, 100 ) )
    BLACK_PAWN   = ( 0x08,  8, ( 920, 200 ) )
    BLACK_KNIGHT = ( 0x09,  2, ( 920, 300 ) )
    BLACK_ROOK   = ( 0x0a,  2, ( 920, 400 ) )
    BLACK_BISHOP = ( 0x0b,  2, ( 920, 500 ) )
    BLACK_KING   = ( 0x0c,  1, ( 920, 600 ) )
    WHITE_COIN   = ( 0x0d, 12, (  10, 800 ) )
    BLACK_COIN   = ( 0x0e, 12, ( 920, 800 ) )


""" Test DraggableType values to uniqueness """
if not test_uniqueness ( [ value[0] for (_, value) in attributes(DraggableType) ] ):
    raise Exception ("Sorry, DraggableType has duplicate values")


DRAGGABLES_NUM = sum ( [ value[1] for (_, value) in attributes(DraggableType) ] )


""" Test DraggableType number to be less than maxnum """
if DRAGGABLES_NUM > 255:
    raise Exception (f"Sorry, DraggableType ({ DRAGGABLES_NUM }) bigger than DRAGGABLE_MAX")


class Draggable:
    maxnum = DRAGGABLES_NUM
    maxdepth = 0


    def __init__ ( self, id, x, y, offsetX, offsetY, owner, type, DBRow, depth ) :
        self.id      = id
        self.x       = x
        self.y       = y
        self.offsetX = offsetX
        self.offsetY = offsetY
        self.owner   = owner
        self.type    = type
        self.DBRow   = DBRow
        self.depth   = depth
        Draggable.maxdepth = depth if depth > Draggable.maxdepth else Draggable.maxdepth


    def to_bytes ( self ) :
        return uint16list_to_bytes([
            self.id,
            self.x,
            self.y,
            self.offsetX,
            self.offsetY,
            self.owner,
            self.type,
            self.depth
        ])


    @classmethod
    def increase_maxdepth ( cls ) :

        cls.maxdepth += 1

        if cls.maxdepth > 0xffff :

            cls.maxdepth = 0

            # TODO:

            raise Exception ( 'need rearrange maxdepth' )


    @classmethod
    def all_to_bytes ( cls, draggables ) :
        b = bytes()
        for d in draggables:
            b = b + d.to_bytes()
        return b


    @classmethod
    def list ( cls, DBRows ) :

        DBRowsUsed = [ False ] * len ( DBRows )

        arr = []

        id = 0

        for ( _, ( type, number, ( x, y ) ) ) in attributes ( DraggableType ) :

            for _ in range ( number ):

                draggable = None

                try :

                    ( i, row ) = [ ( i, row ) for ( i, row ) in enumerate ( DBRows ) if row.type == type and not DBRowsUsed [ i ] ] [ 0 ]

                    # throws IndexError here if such rows are not found

                    DBRowsUsed [ i ] = True

                    draggable = Draggable ( id, row.x, row.y, row.offsetX, row.offsetY, 0, type, row, row.depth )

                except IndexError :

                    draggable = Draggable ( id, x, y, 0, 0, 0, type, None, id )

                arr += [ draggable ]

                id += 1


        unused = [ row for ( i, row ) in enumerate ( DBRows ) if not DBRowsUsed [ i ] ]

        arr += [ Draggable ( id + i, row.x, row.y, row.offsetX, row.offsetY, 0, row.type, row ) for ( i, row ) in enumerate ( unused ) ]

        return arr


    def updateDB ( self ):
        self.DBRow = ( self.DBRow.update if self.DBRow else chessdb.chessdb.createRow ) \
            ( self.x, self.y, self.offsetX, self.offsetY, self.type, self.depth )



class Handler ( WebSocketHandler ) :
    lastTime   = 0  # TODO : from db
    lastOwner  = 0  # TODO : from db
    clients    = set ()
    draggables = None


    @classmethod
    def init ( cls ):
        # At this point db has to be fetched
        cls.draggables = Draggable.list ( chessdb.chessdb.rows )
        cls.lastTime = chessdb.chessdb.lastTime
        cls.lastOwner = chessdb.chessdb.lastOwner


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


    @classmethod
    def updateLastTurn ( cls, time, owner ) :
        cls.lastTime = time
        cls.lastOwner = owner
        chessdb.chessdb.lastTime = cls.lastTime
        chessdb.chessdb.lastOwner = cls.lastOwner
        return uint32list_to_bytes ( [ cls.lastTime ] ) + uint16list_to_bytes ( [ cls.lastOwner ] )


    def on_message (self, message):

        cls = Handler

        message = bytes_to_uint16list(message)

        # init
        if   message [ 0 ] == 1:
            self.id = cls.get_unique_id ()
            lastTurn = cls.updateLastTurn ( cls.lastTime, cls.lastOwner )
            self.send_only_me ( uint16list_to_bytes ( [ 1, self.id ] ) + lastTurn + Draggable.all_to_bytes ( cls.draggables ) )

        # grab
        elif message [ 0 ] == 2:
            id      = message [ 1 ]
            x       = message [ 2 ]
            y       = message [ 3 ]
            offsetX = message [ 4 ]
            offsetY = message [ 5 ]
            if self.id != 0 and cls.draggables[ id ].owner == 0:
                cls.draggables[ id ].owner   = self.id
                cls.draggables[ id ].x       = x
                cls.draggables[ id ].y       = y
                cls.draggables[ id ].offsetX = offsetX
                cls.draggables[ id ].offsetY = offsetY
                cls.draggables[ id ].depth   = Draggable.maxdepth
                Draggable.increase_maxdepth ()
                cls.draggables[ id ].updateDB ()
                self.send_everyone_except_me ( uint16list_to_bytes ( [ 2 ] ) + cls.draggables[ id ].to_bytes () )
            else:
                self.send_only_me ( uint16list_to_bytes ( [ 2 ] ) + cls.draggables[ id ].to_bytes () )

        # put
        elif message [ 0 ] == 3:
            id = message [ 1 ]
            x  = message [ 2 ]
            y  = message [ 3 ]
            if self.id != 0 and cls.draggables[ id ].owner == self.id:
                cls.draggables[ id ].owner   = 0
                cls.draggables[ id ].x       = x
                cls.draggables[ id ].y       = y
                cls.draggables[ id ].updateDB ()
                lastTurn = cls.updateLastTurn ( int ( time.time () ), self.id )
                self.send_everyone_except_me(uint16list_to_bytes([3]) + lastTurn + cls.draggables[id].to_bytes())
            else:
                lastTurn = cls.updateLastTurn ( cls.lastTime, cls.lastOwner )
                self.send_only_me(uint16list_to_bytes([3]) + lastTurn + cls.draggables[id].to_bytes())

        # move
        elif message[0] == 4:
            id = message [ 1 ]
            x  = message [ 2 ]
            y  = message [ 3 ]
            if self.id != 0 and cls.draggables[ id ].owner == self.id:
                cls.draggables[ id ].x = x
                cls.draggables[ id ].y = y
                cls.draggables[ id ].updateDB ()
                self.send_everyone_except_me ( uint16list_to_bytes ( [ 4 ] ) + cls.draggables[ id ].to_bytes () )
            else:
                self.send_only_me ( uint16list_to_bytes ( [ 4 ] ) + cls.draggables[ id ].to_bytes () )

        # err
        else:
            print ( f'Incorrect action: { message [ 0 ] }, { message }' )
