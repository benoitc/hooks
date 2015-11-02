

# Module hooks #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-hook">hook()</a> ###


<pre><code>
hook() = {atom(), atom(), non_neg_integer()} | {atom(), atom(), non_neg_integer(), integer()}
</code></pre>




### <a name="type-hookname">hookname()</a> ###


<pre><code>
hookname() = any()
</code></pre>




### <a name="type-hooks">hooks()</a> ###


<pre><code>
hooks() = [{<a href="#type-hookname">hookname()</a>, [<a href="#type-hook">hook()</a>]}]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-2">all/2</a></td><td>execute all hooks for this HookName and return all results.</td></tr><tr><td valign="top"><a href="#all_till_ok-2">all_till_ok/2</a></td><td>execute all hooks for the HookName until one return ok or {ok, Val}.</td></tr><tr><td valign="top"><a href="#disable_plugin-1">disable_plugin/1</a></td><td>disable a plugin.</td></tr><tr><td valign="top"><a href="#enable_plugin-1">enable_plugin/1</a></td><td>enable a plugin
This function will start an application if not started and register hooks
from it if none have been registered before.</td></tr><tr><td valign="top"><a href="#enable_plugin-2">enable_plugin/2</a></td><td>enable a plugin and load paths if needed.</td></tr><tr><td valign="top"><a href="#mreg-1">mreg/1</a></td><td>register multiple hooks.</td></tr><tr><td valign="top"><a href="#munreg-1">munreg/1</a></td><td>disable multiple hooks.</td></tr><tr><td valign="top"><a href="#only-2">only/2</a></td><td>call the top priority hook for the HookName.</td></tr><tr><td valign="top"><a href="#reg-3">reg/3</a></td><td>register a <code>Module:Fun/Arity</code> as hook, the function name become the hook name.</td></tr><tr><td valign="top"><a href="#reg-4">reg/4</a></td><td>register <code>Module:Fun/Arity</code> for the hook HookName.</td></tr><tr><td valign="top"><a href="#reg-5">reg/5</a></td><td>register <code>Module:Fun/Arity</code> for the hook HookName with a priority
(default is 0).</td></tr><tr><td valign="top"><a href="#run-2">run/2</a></td><td></td></tr><tr><td valign="top"><a href="#run_fold-3">run_fold/3</a></td><td>foold over all hooks registered for HookName, and return Acc.</td></tr><tr><td valign="top"><a href="#unreg-3">unreg/3</a></td><td>unregister <code>Module:Fun/Arity</code>, the function name is the hook.</td></tr><tr><td valign="top"><a href="#unreg-4">unreg/4</a></td><td>unregister <code>Module:Fun/Arity</code> for the hook HookName.</td></tr><tr><td valign="top"><a href="#unreg-5">unreg/5</a></td><td>unregister <code>Module:Fun/Arity</code> registered for the hook HookName with a
priority.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-2"></a>

### all/2 ###

<pre><code>
all(HookName::<a href="#type-hookname">hookname()</a>, Args::list()) -&gt; [any()]
</code></pre>
<br />

execute all hooks for this HookName and return all results

<a name="all_till_ok-2"></a>

### all_till_ok/2 ###

<pre><code>
all_till_ok(HookName::<a href="#type-hookname">hookname()</a>, Args::list()) -&gt; ok | {ok, any()} | {error, term()}
</code></pre>
<br />

execute all hooks for the HookName until one return ok or {ok, Val}.
Otherwise it will return all other results as error.

<a name="disable_plugin-1"></a>

### disable_plugin/1 ###

<pre><code>
disable_plugin(Application::atom()) -&gt; ok
</code></pre>
<br />

disable a plugin

<a name="enable_plugin-1"></a>

### enable_plugin/1 ###

<pre><code>
enable_plugin(Application::atom()) -&gt; ok | {error, term()}
</code></pre>
<br />

enable a plugin
This function will start an application if not started and register hooks
from it if none have been registered before. Hooks are loaded from the
`hooks` key in the application environnement

<a name="enable_plugin-2"></a>

### enable_plugin/2 ###

<pre><code>
enable_plugin(Application::atom(), Paths::[string()]) -&gt; ok | {error, term()}
</code></pre>
<br />

enable a plugin and load paths if needed

<a name="mreg-1"></a>

### mreg/1 ###

<pre><code>
mreg(Hooks::<a href="#type-hooks">hooks()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

register multiple hooks

<a name="munreg-1"></a>

### munreg/1 ###

<pre><code>
munreg(Hooks::<a href="#type-hooks">hooks()</a>) -&gt; ok
</code></pre>
<br />

disable multiple hooks

<a name="only-2"></a>

### only/2 ###

<pre><code>
only(HookName::<a href="#type-hookname">hookname()</a>, Args::list()) -&gt; any() | hooks_not_found
</code></pre>
<br />

call the top priority hook for the HookName

<a name="reg-3"></a>

### reg/3 ###

<pre><code>
reg(Module::atom(), Fun::atom(), Arity::non_neg_integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

register a `Module:Fun/Arity` as hook, the function name become the hook name.

<a name="reg-4"></a>

### reg/4 ###

<pre><code>
reg(HookName::<a href="#type-hookname">hookname()</a>, Module::atom(), Fun::atom(), Arity::non_neg_integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

register `Module:Fun/Arity` for the hook HookName

<a name="reg-5"></a>

### reg/5 ###

<pre><code>
reg(HookName::<a href="#type-hookname">hookname()</a>, Module::atom(), Fun::atom(), Arity::non_neg_integer(), Priority::integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

register `Module:Fun/Arity` for the hook HookName with a priority
(default is 0)

<a name="run-2"></a>

### run/2 ###

<pre><code>
run(HookName::<a href="#type-hookname">hookname()</a>, Args::list()) -&gt; ok
</code></pre>
<br />

<a name="run_fold-3"></a>

### run_fold/3 ###

<pre><code>
run_fold(HookName::<a href="#type-hookname">hookname()</a>, Args::list(), Acc::any()) -&gt; Acc2::any()
</code></pre>
<br />

foold over all hooks registered for HookName, and return Acc.
interuptiuo can be interupted if the hook return `stop` in thacase the
latest Acc will be returned or `{stop, Acc}`, in the case the Acc will be
returned. In other cases the value returned by the hook will be given to
the next function.

<a name="unreg-3"></a>

### unreg/3 ###

<pre><code>
unreg(Module::atom(), Function::atom(), Arity::non_neg_integer()) -&gt; ok
</code></pre>
<br />

unregister `Module:Fun/Arity`, the function name is the hook

<a name="unreg-4"></a>

### unreg/4 ###

<pre><code>
unreg(HookName::<a href="#type-hookname">hookname()</a>, Module::atom(), Fun::atom(), Arity::non_neg_integer()) -&gt; ok
</code></pre>
<br />

unregister `Module:Fun/Arity` for the hook HookName

<a name="unreg-5"></a>

### unreg/5 ###

<pre><code>
unreg(HookName::<a href="#type-hookname">hookname()</a>, Module::atom(), Fun::atom(), Arity::non_neg_integer(), Priority::integer()) -&gt; ok
</code></pre>
<br />

unregister `Module:Fun/Arity` registered for the hook HookName with a
priority

