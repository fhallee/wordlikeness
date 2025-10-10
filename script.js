document.addEventListener("DOMContentLoaded", function () {
    const auditoryButton = document.getElementById("auditory");
    const orthographicButton = document.getElementById("orthographic");
    const transcriptionsField = document.getElementById("transcriptions-field");

    orthographicButton.addEventListener("click", function(){
        transcriptionsField.style.display = "none";
    })

    auditoryButton.addEventListener("click", function(){
        transcriptionsField.style.display = "block";
    })
});