package Foreign.Android.Sqlite

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import android.database.sqlite.SQLiteOpenHelper
import android.R.id
import Foreign.Android.Sqlite.PsDbHelder
import android.database.Cursor


class PsDbHelder(context: Context, dbName: String, dbVersion: Int, val createSql: String) :
    SQLiteOpenHelper(context, dbName, null, dbVersion) {
    override fun onCreate(db: SQLiteDatabase) {
        db.execSQL(createSql)
    }

    override fun onUpgrade(db: SQLiteDatabase?, oldVersion: Int, newVersion: Int) {
        //TODO
    }

}

val __createDb = {context: Any, version: Any, name: Any, createSql: Any ->
    context as Context; version as Int; name as String; createSql as String
    val dbHelper = PsDbHelder(context, name, version, createSql)
    dbHelper
}

val __execSql = {dbHelper: Any, sql: Any, args: Any ->
    dbHelper as PsDbHelder; sql as String; args as List<String>
    dbHelper.writableDatabase.execSQL(sql, args.toTypedArray())
}

val __columnAsString = {cursor: Any, column: Any ->
    cursor as Cursor; column as String
    cursor.getString(cursor.getColumnIndex(column))
}

val __columnAsInt = {cursor: Any, column: Any ->
    cursor as Cursor; column as String;
    cursor.getInt(cursor.getColumnIndex(column))
}

val __mapCursor = {cursor: Any, f: Any ->
    cursor as Cursor; f as (Cursor) -> (() -> Any);
    val res = mutableListOf<Any>()
    with(cursor) {
        while (moveToNext()) {
            res.add(f(this)())
        }
    }
    res.toList()
}

val __querySql = {dbHelper: Any, sql: Any, args: Any ->
    dbHelper as PsDbHelder; sql as String; args as List<String>

    val cursor = dbHelper.readableDatabase.rawQuery(sql, args.toTypedArray())
    cursor
}

