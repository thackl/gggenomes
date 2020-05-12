function fixAuthData( jQuery ) {
    // Author and date by default are rendered as <h4>
    // This turns them into a single <small> tag that aligns with "Source:"
    var auth = document.getElementsByClassName("author")[0];
    var date = document.getElementsByClassName("date")[0];
    var both = document.createElement('small');

    both.innerHTML = auth.innerHTML + ", " +date.innerHTML + "<br>";
    auth.parentNode.replaceChild(both, auth);
    date.parentNode.removeChild(date);
}
$( document ).ready(fixAuthData)

function addLang( jQuery ) {
    $("div.sourceCode").each(function(i, v){
        var lang = $(this).children("pre").attr("class").split(' ').pop()
        var Lang = lang[0].toUpperCase() + lang.slice(1)
        $(this).prepend('<div class="codetag ' + lang + '">' + Lang + '</div>' +
                        '<div class="codetagspace"></div>')
    })
    
    // document.getElementsByClassName("bash")[0]
    // tag = document.createElement('small');
    // tag.innerHTML = "bash"
    // b.appendChild(tag)
}
$( document ).ready(addLang)
