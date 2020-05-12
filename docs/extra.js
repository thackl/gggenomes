function fixAuthData( jQuery ) {
    // Author and date by default are rendered as <h4>
    // This turns them into a single <small> tag that aligns with "Source:"
    var auth = document.getElementsByClassName("author")[0];
    var date = document.getElementsByClassName("date")[0];
    if(auth && date){
        inner = auth.innerHTML + ", " + date.innerHTML + "<br>"
    }else if(auth){
        inner = auth.innerHTML + "<br>"
    }else if(date){
        inner = date.innerHTML + "<br>"
    }else{
        return
    }

    var both = document.createElement('small');
    both.innerHTML = inner;
    auth.parentNode.replaceChild(both, auth);
    date.parentNode.removeChild(date);
}
$( document ).ready(fixAuthData)

function addLang( jQuery ) {
    $("div.sourceCode").each(function(i, v){
        var lang = $(this).children("pre").attr("class").split(' ').pop()
        var Lang = lang[0].toUpperCase() + lang.slice(1)
        $(this).prepend('<div class="codetag ' + lang + '">' + Lang + ' code</div>' +
                        '<div class="codetagspace"></div>')
    })

    // document.getElementsByClassName("bash")[0]
    // tag = document.createElement('small');
    // tag.innerHTML = "bash"
    // b.appendChild(tag)
}
$( document ).ready(addLang)
