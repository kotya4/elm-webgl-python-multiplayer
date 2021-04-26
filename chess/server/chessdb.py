from tornado import gen
import psycopg2


class Row :
    def __init__ ( self, id, x, y, offsetX, offsetY, type, depth ) :
        self.id      = id
        self.x       = x
        self.y       = y
        self.offsetX = offsetX
        self.offsetY = offsetY
        self.type    = type
        self.depth   = depth
        self.was_changed = False
        self.was_created = False


    def update ( self, x, y, offsetX, offsetY, type, depth ) :
        self.x       = x
        self.y       = y
        self.offsetX = offsetX
        self.offsetY = offsetY
        self.type    = type
        self.depth   = depth
        self.was_changed = True
        return self


class chessdb :
    url  = None
    rows = None
    lastTime = None
    lastOwner = None


    # HACK: any manipulations with rows will raise exception if fetch wasn't called successfuly at least one time


    @classmethod
    def createRow ( cls, x, y, offsetX, offsetY, type, depth ) :
        row = Row ( id, x, y, offsetX, offsetY, type, depth )
        row.was_created = True
        cls.rows += [ row ]
        return row


    @classmethod
    def fetch ( cls, url=None ) :

        cls.url = url or cls.url

        with psycopg2.connect( cls.url, sslmode='allow' ).cursor () as cursor :

            # rows

            cursor.execute ( """
                select
                    id,
                    x,
                    y,
                    offsetX,
                    offsetY,
                    type,
                    depth
                from
                    chess ;
            """ )

            cls.rows = []

            for ( id, x, y, offsetX, offsetY, type, depth ) in cursor :
                cls.rows += [ Row ( id, x, y, offsetX, offsetY, type, depth ) ]

            # general

            cursor.execute ( """
                select
                    lastTime,
                    lastOwner
                from
                    chess_general ;
            """ )

            for ( lastTime, lastOwner ) in cursor :
                cls.lastTime = lastTime
                cls.lastOwner = lastOwner

        print ( 'fetch (ok)' )


    @classmethod
    def commit ( cls, url=None ) :

        cls.url = url or cls.url

        with psycopg2.connect( cls.url, sslmode='allow' ).cursor () as cursor :

            # rows

            for row in cls.rows :

                if row.was_created :

                    cursor.execute ( """
                        insert into chess (
                                x,
                                y,
                                offsetX,
                                offsetY,
                                type,
                                depth
                            )
                            values (
                                %s,
                                %s,
                                %s,
                                %s,
                                %s,
                                %s
                            )
                            returning id ;
                    """, [
                        row.x,
                        row.y,
                        row.offsetX,
                        row.offsetY,
                        row.type,
                        row.depth,
                    ])

                    row.id = cursor.fetchone () [ 0 ];

                    print ( 'insert', row.id )

                elif row.was_changed :

                    try:

                        cursor.execute ( """
                            update chess
                                set
                                    x       = %s,
                                    y       = %s,
                                    offsetX = %s,
                                    offsetY = %s,
                                    type    = %s,
                                    depth   = %s
                                where
                                    id = %s ;
                        """, [
                            row.x,
                            row.y,
                            row.offsetX,
                            row.offsetY,
                            row.type,
                            row.depth,
                            row.id
                        ])

                        print ( 'update', row.id )

                    except psycopg2.errors.NumericValueOutOfRange :

                        print ( 'error updating', row.id )
                        print ( 'row.x', row.x )
                        print ( 'row.y', row.y )
                        print ( 'row.offsetX', row.offsetX )
                        print ( 'row.offsetY', row.offsetY )
                        print ( 'row.type', row.type )
                        print ( 'row.depth', row.depth )

                row.was_created = False
                row.was_changed = False

            # general

            cursor.execute ( """
                update chess_general
                    set
                        lastTime  = %s,
                        lastOwner = %s ;
            """, [
                cls.lastTime,
                cls.lastOwner
            ])

            print ( 'update general (ok)' )

            # commit

            cursor.connection.commit ()


    @classmethod
    async def coroutine ( cls ) :

        delay_before_commit = 60 * 5
        delay_after_commit  = 60 * 5

        while True:

            await gen.sleep ( delay_before_commit )

            cls.commit ()

            await gen.sleep ( delay_after_commit )
