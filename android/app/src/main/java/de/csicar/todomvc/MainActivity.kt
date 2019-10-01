package de.csicar.todomvc

import Foreign.PsRuntime.app
import Foreign.PsRuntime.appRun
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        PS.Main.Module.main.app(this).appRun()
    }
}
