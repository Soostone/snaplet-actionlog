<apply template='_application'>

  <bind tag='pageHeader'>
    Action Log
  </bind>

  <bind tag='pageTagline'>
    history of modifications to your data
  </bind>


  <bind tag='contentSecondary'>
    <h3>Action Log Filter</h3>
    <apply template="${actionlogTemplate}/_form"/>
  </bind>

  <bind tag='contentMain'>
    <apply template="${actionlogTemplate}/_list"/>
  </bind>

</apply>


