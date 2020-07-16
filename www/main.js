// Update the plot size to match window height
function setHeight() {
    var window_height = $(window).height();
    var header_height = $(".main-header").height();

    var boxHeight = window_height - header_height - 53;

    $("#plot_container").height(boxHeight);
    $("#plot").height(boxHeight - 20);
}

$(document).on("shiny:connected", function(event) {
    setHeight();
});

$(window).on("resize", function(){
    setHeight();
});

// Create tooltip
$(document).ready(function() {
    $("#plot").mousemove(function(e) {
        //$("#plot_tooltip").show();
        $("#plot_tooltip").css({
            top: (e.pageY + 5) + "px",
            left: (e.pageX + 5) + "px"
        });
    });
});
