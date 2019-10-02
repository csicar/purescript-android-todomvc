package Foreign.Android.Internal.View


import Foreign.PsRuntime.appRun
import PS.Data.Tuple.Module
import android.app.Activity
import android.content.Context
import android.graphics.Typeface
import android.text.Editable
import android.text.TextWatcher
import android.view.View
import android.view.ViewGroup
import android.view.inputmethod.EditorInfo
import android.widget.*
import androidx.appcompat.app.AppCompatActivity
import androidx.core.text.HtmlCompat
import androidx.core.view.*
import androidx.core.widget.addTextChangedListener
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView
import PS.Android.Attributes.Module as Attributes

val __rId = { ctx: Any, id: Any ->
    ctx as Activity; id as String
    val ressourceId = ctx.resources.getIdentifier(id, "id", ctx.packageName)
    ctx.findViewById<View>(ressourceId)
}

val __rDrawable = { ctx: Any, id: Any ->
    ctx as Activity; id as String
    ctx.resources.getIdentifier(id, "drawable", ctx.packageName)
}

val __linearLayout = { ctx: Any ->
    ctx as Context

    LinearLayout(ctx).also {
        val l = LinearLayout.LayoutParams(
            LinearLayout.LayoutParams.MATCH_PARENT,
            LinearLayout.LayoutParams.WRAP_CONTENT
        )
        it.layoutParams = l
    }
}

val __setOrientation = { layout: Any, vertical: Any ->
    vertical as Boolean; layout as LinearLayout
    layout.orientation = if (vertical) LinearLayout.VERTICAL else LinearLayout.HORIZONTAL

}

val __scrollView = { ctx: Any ->
    ctx as Context
    ScrollView(ctx)
}

val __emptyView = { ctx: Any ->
    ctx as Context
    View(ctx)
}

val __button = { ctx: Any, str: Any ->
    ctx as Context; str as String
    val btn = Button(ctx)
    btn.layoutParams = ViewGroup.LayoutParams(
        ViewGroup.LayoutParams.WRAP_CONTENT,
        ViewGroup.LayoutParams.WRAP_CONTENT
    )
    btn.text = str
    btn
}

val __textView = { ctx: Any, str: Any ->
    ctx as Context; str as String
    val tv = TextView(ctx)
    tv.text = str
    tv
}

val __setWidth = { view: Any, layout: Any ->
    view as View; layout as PS.Android.Attributes.Module._Type_Layout
    if (view.layoutParams == null) {
        view.layoutParams = ViewGroup.LayoutParams(
            ViewGroup.LayoutParams.WRAP_CONTENT,
            ViewGroup.LayoutParams.WRAP_CONTENT
        )
    }
    view.layoutParams.width = when (layout) {
        PS.Android.Attributes.Module._Type_Layout.MatchParent -> ViewGroup.LayoutParams.MATCH_PARENT
        PS.Android.Attributes.Module._Type_Layout.WrapContent -> ViewGroup.LayoutParams.WRAP_CONTENT
    }
}

val __setHeight = { view: Any, layout: Any ->
    view as View; layout as PS.Android.Attributes.Module._Type_Layout
    if (view.layoutParams == null) {
        view.layoutParams = ViewGroup.LayoutParams(
            ViewGroup.LayoutParams.WRAP_CONTENT,
            ViewGroup.LayoutParams.WRAP_CONTENT
        )
    }
    view.layoutParams.height = when (layout) {
        PS.Android.Attributes.Module._Type_Layout.MatchParent -> ViewGroup.LayoutParams.MATCH_PARENT
        PS.Android.Attributes.Module._Type_Layout.WrapContent -> ViewGroup.LayoutParams.WRAP_CONTENT
    }
}

class EditTextListener(val ctx: Context) : EditText(ctx) {
    var listener: TextWatcher? = null
    fun setTextChangedListener(cb: (Editable?) -> Unit) {
        removeTextChangedListener()
        listener = addTextChangedListener { cb(it) }
    }

    fun removeTextChangedListener() {
        if (listener != null) removeTextChangedListener(listener)
    }
}

val __editText = { ctx: Any ->
    ctx as Context
    val et = EditTextListener(ctx)
    et.layoutParams = ViewGroup.LayoutParams(
        ViewGroup.LayoutParams.MATCH_PARENT,
        ViewGroup.LayoutParams.WRAP_CONTENT
    )
    et
}

val __setEditType = { view: Any, ty: Any ->
    view as EditText; ty as Attributes._Type_EditType
    view.imeOptions = when (ty) {
        is Attributes._Type_EditType.ActionDone -> EditorInfo.IME_ACTION_DONE
        is Attributes._Type_EditType.ActionSearch -> EditorInfo.IME_ACTION_SEARCH
        Attributes._Type_EditType.ActionGo -> EditorInfo.IME_ACTION_GO
        Attributes._Type_EditType.ActionSend -> EditorInfo.IME_ACTION_SEND
    }
}

val __setSingleLine = { view: Any, singleLine: Any ->
    view as EditText; singleLine as Boolean
    view.isSingleLine = singleLine
}

val __onSubmit = { editText: Any, cb: Any ->
    editText as EditText; cb as () -> Any
    editText.setOnEditorActionListener { _, actionId, event ->
        if (actionId == EditorInfo.IME_ACTION_DONE) {
            cb()
            true
        } else {
            false
        }
    }
}

val __onTextChange = { editText: Any, cb: Any ->
    editText as EditTextListener; cb as (String) -> (() -> Any)
    editText.setTextChangedListener {
        cb(it.toString())()
    }
}

val __removeOnTextChange = { editText: Any ->
    editText as EditTextListener
    editText.removeTextChangedListener()
}

val __imageView = { ctx: Any, res: Any ->
    ctx as Context; res as Int
    val iv = ImageView(ctx)
    iv.setImageResource(res)
    iv
}

val __checkbox = { ctx: Any ->
    ctx as Context;
    CheckBox(ctx).also {
        it.layoutParams = ViewGroup.LayoutParams(
            ViewGroup.LayoutParams.WRAP_CONTENT,
            ViewGroup.LayoutParams.WRAP_CONTENT
        )
    }
}

val __setChecked = { checkbox: Any, checked: Any ->
    checkbox as CheckBox; checked as Boolean
    checkbox.isChecked = checked
}

val __onChecked = { checkbox: Any, cb: Any ->
    checkbox as CheckBox; cb as (Boolean) -> (() -> Any)
    checkbox.setOnCheckedChangeListener { _, isChecked ->
        println("asd")
        cb(isChecked)()
    }
}

val __getView = { viewGroup: Any, index: Any ->
    viewGroup as ViewGroup; index as Int;
    viewGroup[index]
}

val __addView = { viewGroup: Any, view: Any ->
    viewGroup as ViewGroup; view as View
    viewGroup.addView(view)
}

val __replaceView = { viewGroup: Any, index: Any, view: Any ->
    viewGroup as ViewGroup; index as Int; view as View
    if (index < viewGroup.childCount && view === viewGroup[index]) {
        //already in the right place
    } else {
        viewGroup.removeViewAt(index)
        if (index == viewGroup.childCount) {
            viewGroup.addView(view)
        } else {
            viewGroup.addView(view, index)
        }
    }
}

val __removeView = { viewGroup: Any, index: Any ->
    viewGroup as ViewGroup; index as Int;
    println("remove index $index, ${viewGroup.childCount}")
    viewGroup.getChildAt(index).clearFocus();
    viewGroup.removeViewAt(index)
}

val __addContentView = { ctx: Any, view: Any ->

    ctx as Activity; view as View
    ctx.addContentView(
        view,
        ViewGroup.LayoutParams(
            ViewGroup.LayoutParams.MATCH_PARENT,
            ViewGroup.LayoutParams.MATCH_PARENT
        )
    )
}

val __onClick = { view: Any, f: Any ->
    view as View; f as () -> Unit
    view.setOnClickListener { f() }
}

val __setTextTextView = { textView: Any, str: Any ->
    textView as TextView; str as CharSequence
    if (str != textView.text.toString()) {
        textView.text = str
    }
}

val textToHtml = { html: Any ->
    html as String
    HtmlCompat.fromHtml(html, HtmlCompat.FROM_HTML_MODE_COMPACT)
}

val __getText = { textView: Any ->
    textView as TextView
    textView.text.toString()
}

val __setTextSize = { textView: Any, size: Any ->
    textView as TextView; size as Double
    textView.textSize = size.toFloat()
    Unit
}

val __setTextStyle = { textView: Any, style: Any ->
    textView as TextView; style as PS.Android.Attributes.Module._Type_TextStyle
    textView.typeface = when (style) {
        PS.Android.Attributes.Module._Type_TextStyle.Bold -> Typeface.DEFAULT_BOLD
        PS.Android.Attributes.Module._Type_TextStyle.Normal -> Typeface.DEFAULT
        PS.Android.Attributes.Module._Type_TextStyle.Monospace -> Typeface.MONOSPACE
    }
}

typealias ViewBinder = ((View) -> ((Int) -> ((Any) -> ((Any) -> (() -> View)))))

class PsRecyclerViewAdapter(
    var dataset: List<Any>,
    var oldDataset: List<Any>,
    private val createViewHolder: (Context) -> (() -> Any),
    private val bindView: ViewBinder
) :
    RecyclerView.Adapter<PsRecyclerViewAdapter.PsBinderViewHolder>() {

    class PsBinderViewHolder(val view: View, var uiModel: Any) :
        RecyclerView.ViewHolder(view)


    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): PsBinderViewHolder {
        val res = createViewHolder(parent.context) as Module._Type_Tuple.Tuple
        println("create view (model: ${res.value1}")
        return PsBinderViewHolder(res.value0.appRun() as View, res.value1)
    }

    override fun onBindViewHolder(holder: PsBinderViewHolder, position: Int) {
        println("bind view! (position $position model ${holder.uiModel})")
        val oldModel = holder.uiModel
        holder.uiModel = dataset[position]
        bindView(holder.view)(position)(holder.uiModel)(oldModel)()
    }

    override fun getItemCount() = dataset.size
}

val __recyclerView =
    { ctx: Any, dataset: Any, oldDataset: Any, createViewHolder: Any, bindView: Any ->
        ctx as Context
        createViewHolder as ((Context) -> (() -> Any))
        dataset as List<Any>
        oldDataset as List<Any>
        bindView as ViewBinder


        RecyclerView(ctx).apply {
            layoutManager = LinearLayoutManager(ctx)
            adapter = PsRecyclerViewAdapter(dataset, oldDataset, createViewHolder, bindView)
        }
    }

val __updateRecyclerView = { view: Any, newVal: Any ->
    view as RecyclerView; newVal as List<Any>
    val adapter = view.adapter as PsRecyclerViewAdapter
    adapter.dataset = newVal
    if (!view.isComputingLayout) {

        adapter.notifyDataSetChanged()
    }
}

val __updateRecyclerViewFrom = { view: Any, newVal: Any, oldVal: Any ->
    view as RecyclerView; newVal as List<Any>; oldVal as List<Any>
    val adapter = view.adapter as PsRecyclerViewAdapter
    adapter.dataset = newVal
    adapter.oldDataset = oldVal
    println("updateRecyclerViewFrom ${newVal.size}")
    if (!view.isComputingLayout) {
        adapter.notifyDataSetChanged()
        println("update done")
    }
}

val __setMargins = { view: Any, left: Any, top: Any, right: Any, bottom: Any ->
    view as View; left as Int; top as Int; right as Int; bottom as Int
    if (view.layoutParams == null) {
        view.layoutParams = ViewGroup.MarginLayoutParams(
            ViewGroup.LayoutParams.WRAP_CONTENT,
            ViewGroup.LayoutParams.WRAP_CONTENT
        )
    }
    (view.layoutParams as ViewGroup.MarginLayoutParams).leftMargin = left
    (view.layoutParams as ViewGroup.MarginLayoutParams).topMargin = top
    (view.layoutParams as ViewGroup.MarginLayoutParams).rightMargin = right
    (view.layoutParams as ViewGroup.MarginLayoutParams).bottomMargin = bottom
    Unit
}

val __getActionBar = { ctx: Any ->
    ctx as AppCompatActivity;
    ctx.supportActionBar!!
}

val __setActionBarTitle = { actionBar: Any, title: Any ->
    actionBar as androidx.appcompat.app.ActionBar; title as String
    actionBar.title = title
}