<apply template='_application'>

  <bind tag='pageHeader'>
    Item Details
  </bind>

  <bind tag='contentSecondary'>
    <div class="alert alert-info">
      <p>Details for a single action log item</p>
    </div>
  </bind>
  
  <bind tag='contentMain'>
    <apply template="${actionlogTemplate}/_details"/>
  </bind>

</apply>


