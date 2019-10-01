package Foreign.Android.Render

import PS.Android.Ui.Module
import android.app.Activity
import android.content.Context
import android.view.View
import android.view.ViewGroup
import androidx.core.view.get

val setContentView = { view: Any ->
    { ctx: Any ->

        {
            ctx as Activity; view as View
            if (getContent(ctx) === view) {
                println("nothing to do: go the same")
            } else {

                ctx.setContentView(view)
            }
        }
    }

}

val getContent = { ctx: Any ->
    {
        ctx as ViewGroup;
        if (ctx.childCount == 0) {
            View(ctx.context)
        } else {
            ctx[0]
        }
    }
}

val setContent = { viewGroup: Any ->
    { view: Any ->
        {
            viewGroup as ViewGroup; view as View
            if (viewGroup.childCount > 0 && viewGroup.getChildAt(0) === view) {
                println("reuse view")
            } else {
                if (viewGroup.childCount > 0) {
                    viewGroup.removeViewAt(0)
                }
                viewGroup.addView(view)
            }
        }
    }
}