window.addEventListener("DOMContentLoaded", function() {
	// attach click handler for '...' links to expand/collapse commit descriptions
    var expanders = document.querySelectorAll(".commit-group-item .hidden-text-expander a");
    var expand = function(event)
    {
        var desc = event.target.parentNode.parentNode.parentNode.querySelector('.commit-desc');
        if (desc.style.display === 'block')
        {
            desc.style.display = 'none';
        }
        else
        {
            desc.style.display = 'block';
        }
    };
    for (var i=0, ii=expanders.length; i<ii; i++)
    {
        expanders[i].onclick = expand;
    }
}, false);
