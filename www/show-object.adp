<master>
<property name="doc(title)">@title;literal@</property>
<property name="context">@context;literal@</property>
<property name="head">
<style>
pre.code {
        font-size: 80%;
        font-family: courier, monospace;
        padding-right: 10px;
        padding-left: 10px;
        padding-bottom: 10px;
        padding-top: 10px;
        border: #cccccc 1px solid;
        background-color: #eeeeee;
        margin-bottom: 15px;
}
pre em {
        font-family: cursive;
        color: #888888;
}
pre tt {
        font-family: helvetica;
        font-weight: 900;
}
pre it {
        font-style: italic;
        color: green;
}
dd {margin-left: 2em;}
</style>
</property>

<div style='font-size: 70%'>
@dimensional_slider;noquote@
</div>
<hr>

<if @class_hierarchy@ not nil>
<if @svg@ not nil>
@svg;literal@
</if>
<else>
<img style='float: right; max-width: 800px;' src='./show-class-graph?classes=@class_hierarchy@&amp;documented_only=@documented_only@'>
</else>
</if>

@output;literal@

<hr>
<div style='font-size: 70%'>
@dimensional_slider;noquote@
</div>
