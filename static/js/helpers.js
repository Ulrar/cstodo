$(function()
{
        delHelper();
//       copyHelper();
});

function delHelper()
{
  $("a.delete").click(function()
  {
    $.ajax(
    {
      type: "DELETE",
      url: $(this).attr("data-url"),
      success: function(data, status, xhr)
      {
        location.reload();
      }
    });
    return false;
  });
}

/*function copyHelper()
{
  $("a.copy").click(function()
  {
    var name = prompt('Name ?');
    $.ajax(
    {
      type: "GET",
      url: $(this).attr("data-url") + name,
      success: function(data, status, xhr)
      {
        window.location.replace("/lists");
      }
    });
  return false;
  });
}*/
