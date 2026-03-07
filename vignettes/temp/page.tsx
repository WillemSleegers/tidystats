import { Section } from "@/components/Section";

export default () => {
  return (
    <>
      <Section gray>
        <img
          className="mx-auto"
          src="/assets/images/word.svg"
          alt="Microsoft Word logo"
          style={{ maxWidth: 160, maxHeight: 160 }}
        />
      </Section>

      <div id="content">
        <h1 className="text-center">Word add-in</h1>

        <p>
          The tidystats Word add-in can be used to report statistics in a
          Microsoft Word document, using a file created with the tidystats R
          package.
        </p>

        <h2>Main features</h2>

        <ul>
          <li>
            A user-friendly point-and-click interface to insert statistics in
            APA-style
          </li>
          <li>Insert multiple statistics with a single click</li>
          <li>
            Customize which statistics to include when inserting multiple
            statistics
          </li>
          <li>Update reported statistics with new statistics in one go</li>
        </ul>

        <h2>Installation</h2>

        <p>
          The tidystats Word add-in is available in the Office Add-in store. You
          can find this store in your Word document by going to the Add-in
          section of the Insert tab. Simply search for 'tidystats' and you
          should find the add-in.
        </p>

        <p>
          Once installed, a button saying 'Insert Statistics' will be added to
          the Insert tab of your Word document.
        </p>

        <h2>Usage</h2>

        <p>
          Using the tidystats add-in is relatively straightforward. Open the
          tidystats add-in by clicking on the 'Insert Statistics' button found
          in the Insert pane of your Word document. After tidystats opens, click
          on 'Upload statistics' to select the file created with the tidystats R
          package. This will reveal a list of analyses. Click on the dropdown
          arrows to reveal the statistics of each analysis.
        </p>

        <img
          src="/assets/images/screenshot.png"
          alt="tidystats add-in screenshot"
        />

        <p>
          Click on the plus icon next to an individual statistic to insert that statistic or click on the plus icon next to 'Statistics:' to insert multiple statistics into your document, at the location of your cursor.
        </p>

        <p>
          It is possible to customize exactly which statistics will be reported
          when inserting multiple statistics. Click on the gear icon next to
          'Statistics:' to reveal checkboxes next to each statistic. By default,
          all checkboxes are checked. Unchecking a checkbox will prevent that
          statistic from being inserted when inserting multiple statistics at
          once.
        </p>

        <p>
          To update reported statistics, click on the 'Update statistics' button in the Actions tab after uploading a new file with corrected statistics. This will automatically update all reported statistics with the statistics from the new file that have the same identifier.
        </p>

        <h2>An example</h2>

        <p>
          Below is an example of how to use several features of the tidystats
          add-in.
        </p>

        <video controls className="rounded-md shadow">
          <source src="/assets/video/example.mp4" type="video/mp4" />
        </video>

        <h2>Support</h2>

        <p>
          Do you have a question or comment, such as a feature request, about
          tidystats? Check out the <a href="/support/">support</a> page for ways
          to contact me.
        </p>
      </div>
    </>
  );
};
