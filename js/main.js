document.getElementById("menubar")
  .getElementsByClassName("icon")[0]
  .addEventListener("click", () => {
    document.getElementById("menubar")
      .getElementsByTagName("nav")[0]
      .classList
      .toggle("hidden");
    document.getElementById("menubar")
      .getElementsByClassName("icon")[0]
      .classList
      .toggle("clicked");
  });
