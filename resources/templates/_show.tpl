<apply template='_application'>

  <bind tag='pageHeader'>
    Item Details
  </bind>

  <bind tag='contentSecondary'>
  </bind>
  
  <bind tag='contentMain'>
  <actionLogView>
    
    <div class="row-fluid">
      <div class="span6">

        <h4>Name</h4>
        <p><actionLogName/></p>

        <h4>Description</h4>
        <p><actionLogDescription/></p>

        <h4>Code</h4>
        <p><actionLogCode/></p>

        <h4>Unit</h4>
        <p><actionLogUnit/></p>

        <h4>Created by</h4>
        <p><actionLogCreatedBy/></p>

        <h4>Created at</h4>
        <p><actionLogCreatedAt/></p>

        <h4>Updated at</h4>
        <p><actionLogUpdatedAt/></p>

        <h4>Active?</h4>
        <p><actionLogIsActive/></p>
      </div>

    </div>

    <div class="form-actions">
      <a href="/actionlog" class="btn">Return To Listing</a>
    </div>


    
  </actionLogView>
  </bind>

</apply>


