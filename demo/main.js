console.info("Statistical Learning Project");
const coefficients = [
  11.935597411,
  -0.058800602,
  -0.167956847,
  0.011825006,
  0.020423041,
  0.005662866,
  0.022764411,
  0.214688611,
  0.005394128,
  -0.055279253,
  0.034185375,
  -1.08221879,
  0.475246363,
  0.014321501,
];
mdc.ripple.MDCRipple.attachTo(document.querySelector(".foo-button"));
[].map.call(document.querySelectorAll(".mdc-switch"), function (el) {
  return new mdc.switchControl.MDCSwitch(el);
});
[].map.call(document.querySelectorAll(".mdc-text-field"), function (el) {
  return new mdc.textField.MDCTextField(el);
});
const select = new mdc.select.MDCSelect(document.querySelector(".mdc-select"));
league_value = 0;
select.listen("MDCSelect:change", () => {
  switch (select.value) {
    case "bundesliga":
      league_value = 0;
      break;
    case "la-liga":
      league_value = coefficients[9];
      break;
    case "ligue-1":
      league_value = coefficients[10];
      break;
    case "other":
      league_value = coefficients[11];
      break;
    case "premier-league":
      league_value = coefficients[12];
      break;
    case "serie-a":
      league_value = coefficients[13];
      break;
  }
});

function getMarketValue() {
  console.info("Market Value");
  const age = parseInt(document.getElementById("age").value);
  const height = parseInt(document.getElementById("height").value);
  const contract = parseInt(document.getElementById("contract").value);
  const offensive = document.getElementById("offensive").checked;
  const games = parseInt(document.getElementById("games").value);
  const goals = parseInt(document.getElementById("goals").value);
  const assists = parseInt(document.getElementById("assists").value);
  const yellows = parseInt(document.getElementById("yellows").value);
  let league_value = 0;
  let market_value =
    coefficients[0] + age * coefficients[1] + offensive
      ? coefficients[2]
      : 0 +
        height * coefficients[3] +
        games * coefficients[4] +
        goals * coefficients[5] +
        assists * coefficients[6] +
        contract * coefficients[7] +
        yellows * coefficients[8] +
        league_value;
  console.info(offensive, market_value);
}
