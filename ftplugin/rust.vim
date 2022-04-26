" Language:     Rust
" Description:  Vim ftplugin for Rust
" Maintainer:   Chris Morgan <me@chrismorgan.info>
" Last Change:  June 08, 2016
" For bugs, patches and license go to https://github.com/rust-lang/rust.vim

if exists("b:did_ftplugin")
    finish
endif
let b:did_ftplugin = 1

" vint: -ProhibitAbbreviationOption
let s:save_cpo = &cpo
set cpo&vim
" vint: +ProhibitAbbreviationOption

if get(b:, 'current_compiler', '') ==# ''
    if strlen(findfile('Cargo.toml', '.;')) > 0
        compiler cargo
    else
        compiler rustc
    endif
endif

" Variables {{{1

" The rust source code at present seems to typically omit a leader on /*!
" comments, so we'll use that as our default, but make it easy to switch.
" This does not affect indentation at all (I tested it with and without
" leader), merely whether a leader is inserted by default or not.
if get(g:, 'rust_bang_comment_leader', 0)
    " Why is the `,s0:/*,mb:\ ,ex:*/` there, you ask? I don't understand why,
    " but without it, */ gets indented one space even if there were no
    " leaders. I'm fairly sure that's a Vim bug.
    setlocal comments=s1:/*,mb:*,ex:*/,s0:/*,mb:\ ,ex:*/,:///,://!,://
else
    setlocal comments=s0:/*!,ex:*/,s1:/*,mb:*,ex:*/,:///,://!,://
endif
setlocal commentstring=//%s
setlocal formatoptions-=t formatoptions+=croqnl
" j was only added in 7.3.541, so stop complaints about its nonexistence
silent! setlocal formatoptions+=j

" smartindent will be overridden by indentexpr if filetype indent is on, but
" otherwise it's better than nothing.
setlocal smartindent nocindent

if get(g:, 'rust_recommended_style', 1)
    let b:rust_set_style = 1
    setlocal shiftwidth=4 softtabstop=4 expandtab
    setlocal textwidth=99
endif

setlocal include=\\v^\\s*(pub\\s+)?use\\s+\\zs(\\f\|:)+
setlocal includeexpr=rust#IncludeExpr(v:fname)

setlocal suffixesadd=.rs

if exists("g:ftplugin_rust_source_path")
    let &l:path=g:ftplugin_rust_source_path . ',' . &l:path
endif

if exists("g:loaded_delimitMate")
    if exists("b:delimitMate_excluded_regions")
        let b:rust_original_delimitMate_excluded_regions = b:delimitMate_excluded_regions
    endif

    augroup rust.vim.DelimitMate
        autocmd!

        autocmd User delimitMate_map   :call rust#delimitmate#onMap()
        autocmd User delimitMate_unmap :call rust#delimitmate#onUnmap()
    augroup END
endif

" Integration with auto-pairs (https://github.com/jiangmiao/auto-pairs)
if exists("g:AutoPairsLoaded") && !get(g:, 'rust_keep_autopairs_default', 0)
    let b:AutoPairs = {'(':')', '[':']', '{':'}','"':'"', '`':'`'}
endif

if has("folding")
    " The number of curly braces we’re inside at the specified position.
    function! s:BraceLevel(lnum, cnum) abort
        let braces_level = 0
        for id in synstack(a:lnum, a:cnum)
            let name = synIDattr(id, "name")
            if name ==# "rustFoldBraces"
                let braces_level += 1
            endif
        endfor
        return braces_level
    endfunction

    " [DISABLED: " Returns -1 for empty lines,]
    " Returns 1 for outer attribute lines,
    " Returns 0 for everything else.
    function! s:IsOuterAttribute(lnum) abort
        " The last character is a decent place to look. Not perfect, as there
        " might be whitespace or a comment after, but close enough.
        let last_char_col = col([a:lnum, "$"]) - 1
        if last_char_col == 0
            return 0  " -1
        else
        for id in synstack(a:lnum, last_char_col)
            let name = synIDattr(id, "name")
            if name ==# "rustCommentLineDocOuter" ||
                    \ name ==# "rustCommentBlockDocOuter" ||
                    \ name ==# "rustAttributeOuter"
                return 1
            endif
        endfor
        return 0
    endfunction

    " Returns:
    "
    " • [n, -1] for empty lines that aren’t inside an outer attribute.
    " • [n, 1] for outer attribute lines for level n+1.
    " • FIXME: [n, 0] for lines like `fn foo(` that don’t contain the
    "   brace, but I’d rather like to detect such keywords and bump subsequent
    "   lines until the brace to [n+1, 0].
    " • [n+1, 0] for lines containing the brace of the next level,
    "   including `fn foo() {`, `{`, and `}` provided it has no trailing
    "   comment.
    function! s:FoldInfo(lnum) abort
        let attribute = s:IsOuterAttribute(a:lnum)
        let start_level = s:BraceLevel(a:lnum, 1)
        let end_level = s:BraceLevel(a:lnum, col([a:lnum, "$"]))
        return [max([start_level, end_level]), attribute]
    endfunction

    " rustFoldBraces gives us a basic fold level, but we want to improve it to
    " group attributes with the following item.
    " • Is the first non-whitespace character on this line an outer attribute
    "   (including doc comments)? Boost the level by one.
    function! RustFold() abort
        let [braces_level, attribute] = s:FoldInfo(v:lnum)
        return braces_level + attribute
    endfunction

    function! RustFoldText() abort
        let fs = v:foldstart
        while fs <= v:foldend
            let [braces_level, attribute] = s:FoldInfo(fs)
            if braces_level == 0 || attribute
                let fs = fs + 1
                continue
            endif
            break
        endwhile

        " TODO: join lines until rustFoldBraces level increases.
        " (But this will only be any use once I sort out keeping `fn foo(` on
        " the same foldlevel as its matching `{`.)
        if fs > v:foldend
            " Undesirable, but currently possible on the likes of `fn foo(`
            " where the brace is on a subsequent line.
            let fs = v:foldstart
        endif
        let line = getline(fs)
        if line =~# '{$'
            if getline(nextnonblank(fs + 1)) =~# '^\s*}'
                let line = line . '}'
            else
                let line = line . ' … }'
            endif
        endif

        let width = winwidth(0) - &foldcolumn
            \ - (&number ? max([4, float2nr(log10(line('$'))) + 2]) : 0)
        if has("signs")
            if &signcolumn ==# "auto" || &signcolumn ==# "yes"
                let width = width - 2
            endif
        endif

        if 1 " right-align the number-of-lines-in-fold suffix
            let line = line . ' '
            let suffix = ' ' . (v:foldend - v:foldstart + 1) . ' lines '
            let suffix_width = strdisplaywidth(suffix)
            let fill = repeat('-', width - strdisplaywidth(line) - suffix_width)
            " FIXME: strcharpart is wrong if any character spans multiple
            " columns (e.g. Tab, CJK, most emoji).
            return strcharpart(line . fill, 0, width - suffix_width) . suffix
        else " left-align suffix
            let suffix = ' [' . (v:foldend - v:foldstart + 1) . ' lines] '
            let suffix_width = strdisplaywidth(suffix)
            if strdisplaywidth(line) + suffix_width > width
                " FIXME: strcharpart is wrong and insufficient if any character
                " spans multiple columns (e.g. Tab, CJK, most emoji), but I
                " didn’t quite want to do the while-too-long-pop dance.
                let line = strcharpart(line, 0, width - suffix_width - 2) . ' …'
            endif
            return line . suffix
        endif
    endfunction

    if get(g:, 'rust_fold', 0)
        let b:rust_set_foldmethod=1
        setlocal foldmethod=syntax
        setlocal foldexpr=RustFold()
        setlocal foldtext=RustFoldText()
        if g:rust_fold == 2
            setlocal foldlevel<
        else
            setlocal foldlevel=99
        endif
    endif
endif

if has('conceal') && get(g:, 'rust_conceal', 0)
    let b:rust_set_conceallevel=1
    setlocal conceallevel=2
endif

" Motion Commands {{{1

" Bind motion commands to support hanging indents
nnoremap <silent> <buffer> [[ :call rust#Jump('n', 'Back')<CR>
nnoremap <silent> <buffer> ]] :call rust#Jump('n', 'Forward')<CR>
xnoremap <silent> <buffer> [[ :call rust#Jump('v', 'Back')<CR>
xnoremap <silent> <buffer> ]] :call rust#Jump('v', 'Forward')<CR>
onoremap <silent> <buffer> [[ :call rust#Jump('o', 'Back')<CR>
onoremap <silent> <buffer> ]] :call rust#Jump('o', 'Forward')<CR>

" Commands {{{1

" See |:RustRun| for docs
command! -nargs=* -complete=file -bang -buffer RustRun call rust#Run(<bang>0, <q-args>)

" See |:RustExpand| for docs
command! -nargs=* -complete=customlist,rust#CompleteExpand -bang -buffer RustExpand call rust#Expand(<bang>0, <q-args>)

" See |:RustEmitIr| for docs
command! -nargs=* -buffer RustEmitIr call rust#Emit("llvm-ir", <q-args>)

" See |:RustEmitAsm| for docs
command! -nargs=* -buffer RustEmitAsm call rust#Emit("asm", <q-args>)

" See |:RustPlay| for docs
command! -range=% RustPlay :call rust#Play(<count>, <line1>, <line2>, <f-args>)

" See |:RustFmt| for docs
command! -bar -buffer RustFmt call rustfmt#Format()

" See |:RustFmtRange| for docs
command! -range -buffer RustFmtRange call rustfmt#FormatRange(<line1>, <line2>)

" See |:RustInfo| for docs
command! -bar RustInfo call rust#debugging#Info()

" See |:RustInfoToClipboard| for docs
command! -bar RustInfoToClipboard call rust#debugging#InfoToClipboard()

" See |:RustInfoToFile| for docs
command! -bar -nargs=1 RustInfoToFile call rust#debugging#InfoToFile(<f-args>)

" See |:RustTest| for docs
command! -buffer -nargs=* -count -bang RustTest call rust#Test(<q-mods>, <count>, <bang>0, <q-args>)

if !exists("b:rust_last_rustc_args") || !exists("b:rust_last_args")
    let b:rust_last_rustc_args = []
    let b:rust_last_args = []
endif

" Cleanup {{{1

let b:undo_ftplugin = "
            \ setlocal formatoptions< comments< commentstring< include< includeexpr< suffixesadd<
            \|if exists('b:rust_set_style')
                \|setlocal tabstop< shiftwidth< softtabstop< expandtab< textwidth<
            \|endif
            \|if exists('b:rust_original_delimitMate_excluded_regions')
                \|let b:delimitMate_excluded_regions = b:rust_original_delimitMate_excluded_regions
                \|unlet b:rust_original_delimitMate_excluded_regions
            \|else
                \|unlet! b:delimitMate_excluded_regions
            \|endif
            \|if exists('b:rust_set_foldmethod')
                \|setlocal foldmethod< foldexpr< foldtext< foldlevel<
                \|unlet b:rust_set_foldmethod
            \|endif
            \|if exists('b:rust_set_conceallevel')
                \|setlocal conceallevel<
                \|unlet b:rust_set_conceallevel
            \|endif
            \|unlet! b:rust_last_rustc_args b:rust_last_args
            \|delcommand RustRun
            \|delcommand RustExpand
            \|delcommand RustEmitIr
            \|delcommand RustEmitAsm
            \|delcommand RustPlay
            \|nunmap <buffer> [[
            \|nunmap <buffer> ]]
            \|xunmap <buffer> [[
            \|xunmap <buffer> ]]
            \|ounmap <buffer> [[
            \|ounmap <buffer> ]]
            \|setlocal matchpairs-=<:>
            \|unlet b:match_skip
            \"

" }}}1

" Code formatting on save
augroup rust.vim.PreWrite
    autocmd!
    autocmd BufWritePre *.rs silent! call rustfmt#PreWrite()
augroup END

setlocal matchpairs+=<:>
" For matchit.vim (rustArrow stops `Fn() -> X` messing things up)
let b:match_skip = 's:comment\|string\|rustCharacter\|rustArrow'

" vint: -ProhibitAbbreviationOption
let &cpo = s:save_cpo
unlet s:save_cpo
" vint: +ProhibitAbbreviationOption

" vim: set et sw=4 sts=4 ts=8:
